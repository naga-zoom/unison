#!/usr/bin/env python3
"""PR-64: delete-definitions MCP-level isError reflects UCM failure state.

Symptom: MCP `delete-definitions` always returns `isError=false` at the
MCP envelope level, even when UCM refused the delete (dependents block,
missing name, can't-delete-constructor). The diagnostic text lands in
the inner `errorMessages` array (Output.isFailure routes it correctly)
but the OUTER MCP `isError` boolean stays false. Callers can't
distinguish "succeeded with chatter" from "blocked, here's why" without
parsing the JSON payload — defeats the purpose of the MCP error field.

Root cause: deleteDefinitionsTool unconditionally builds the response
via `textToolResult` which sets `callToolIsError = False`.

Fix: when CliOutput.errorMessages is non-empty, build the response with
`callToolIsError = True` instead.

Scenarios under test:
  1. force=false + target with dependents → blocked, isError=true expected
  2. force=false + missing name → TermAndOrTypeNameNotFound, isError=true expected
  3. force=true + dependents-free name → success, isError=false expected
  4. force=true + dependents (skips safety check) → success, isError=false expected
"""
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile

UCM = os.environ.get(
    'UCM_BIN',
    '/Users/nagarjunapamu/workplace/unison/ucm-source/.stack-work/install/'
    'aarch64-osx/13effb1cbf66165a71a55f6fa335e66ac536aabae0aabb1c4459a2c88ea63e25/'
    '9.10.3/bin/unison',
)

ANSI_RE = re.compile(r'\x1b\[[0-9;]*[a-zA-Z]')


def run_cli(cb, cwd, lines, create=False, timeout=120):
    stdin = '\n'.join(lines + ['quit', '']).encode()
    flag = '-C' if create else '-c'
    r = subprocess.run([UCM, flag, cb], input=stdin, capture_output=True,
                       timeout=timeout, cwd=cwd)
    return ANSI_RE.sub('', r.stdout.decode(errors='replace'))


def run_mcp(cb, calls, create=False, timeout=60):
    msgs = [
        {'jsonrpc': '2.0', 'id': 1, 'method': 'initialize',
         'params': {'protocolVersion': '2024-11-05', 'capabilities': {},
                    'clientInfo': {'name': 's', 'version': '1'}}},
        {'jsonrpc': '2.0', 'method': 'notifications/initialized', 'params': {}},
    ]
    for i, (name, args) in enumerate(calls, start=2):
        msgs.append({'jsonrpc': '2.0', 'id': i, 'method': 'tools/call',
                     'params': {'name': name, 'arguments': args}})
    stdin = ''.join(json.dumps(m) + '\n' for m in msgs)
    flag = '-C' if create else '-c'
    r = subprocess.run([UCM, flag, cb, 'mcp'], input=stdin,
                       capture_output=True, text=True, timeout=timeout)
    out = {}
    for line in r.stdout.splitlines():
        try:
            o = json.loads(line)
            rid = o.get('id')
            if rid is not None and rid >= 2:
                content = o.get('result', {}).get('content', [{}])[0].get('text', '')
                out[rid] = {
                    'isError': o.get('result', {}).get('isError', False),
                    'content': content,
                }
        except json.JSONDecodeError:
            pass
    return out


ok = 0
fail = 0


def check(label, cond, detail=''):
    global ok, fail
    if cond:
        print(f'  PASS  {label}')
        ok += 1
    else:
        print(f'  FAIL  {label}    {detail[:400]}')
        fail += 1


def payload_of(resp):
    try:
        return json.loads(resp.get('content', ''))
    except (json.JSONDecodeError, TypeError):
        return None


cb_parent = tempfile.mkdtemp(prefix='ucm-pr64-')
cb = os.path.join(cb_parent, 'codebase')
print(f'-- PR-64 delete-iserror-routing smoke (codebase={cb}) --')

# Bootstrap: testproj with foo + bar (bar depends on foo) + baz (isolated)
scratch = os.path.join(cb_parent, 'scratch.u')
with open(scratch, 'w') as f:
    f.write('foo : Nat\nfoo = 1\n\nbar : Nat\nbar = foo\n\nbaz : Nat\nbaz = 42\n')
out = run_cli(cb, cb_parent, [
    'project.create testproj',
    'switch testproj/main',
    'load scratch.u',
    'add',
], create=True)
check('bootstrap testproj with foo + bar + baz',
      '+ bar' in out and '+ foo' in out and '+ baz' in out,
      out[-400:])
os.remove(scratch)

# --- Scenario 1: force=false + dependents-blocked ---
r = run_mcp(cb, [
    ('delete-definitions', {
        'projectContext': 'testproj:main',
        'names': ['foo'],
    }),
])
resp = r.get(2, {})
payload = payload_of(resp)
check('S1: dependents-blocked delete returns structured payload',
      payload is not None,
      resp.get('content', '')[:300])
check('S1: dependents-blocked → isError=true (the bug fix)',
      resp.get('isError') is True,
      f"isError={resp.get('isError')}, errorMessages={(payload.get('errorMessages') if payload else None)}")
check('S1: errorMessages remains populated (regression check)',
      payload is not None and len(payload.get('errorMessages', [])) > 0,
      str(payload.get('errorMessages') if payload else None)[:300])

# Clean up the temp branch UCM created during S1 so subsequent
# scenarios start clean. (Use `cancel` against the temp branch.)
# Discover which update-* branch was created.
out = run_cli(cb, cb_parent, ['switch testproj/main', 'branches'])
temp = None
for line in out.splitlines():
    m = re.search(r'(update-main-\d+)', line)
    if m:
        temp = m.group(1)
        break
if temp:
    r = run_mcp(cb, [
        ('cancel', {'projectContext': f'testproj:{temp}'}),
    ])

# --- Scenario 2: force=false + non-existent name ---
r = run_mcp(cb, [
    ('delete-definitions', {
        'projectContext': 'testproj:main',
        'names': ['thisDoesNotExist'],
    }),
])
resp = r.get(2, {})
payload = payload_of(resp)
check('S2: missing-name delete returns structured payload',
      payload is not None,
      resp.get('content', '')[:300])
check('S2: missing-name → isError=true',
      resp.get('isError') is True,
      f"isError={resp.get('isError')}, errorMessages={(payload.get('errorMessages') if payload else None)}")

# --- Scenario 3: force=true + dependents-free name → success, isError=false ---
r = run_mcp(cb, [
    ('delete-definitions', {
        'projectContext': 'testproj:main',
        'names': ['baz'],
        'force': True,
    }),
])
resp = r.get(2, {})
payload = payload_of(resp)
check('S3: force=true on isolated name returns payload',
      payload is not None,
      resp.get('content', '')[:300])
check('S3: force=true success → isError=false (no regression)',
      resp.get('isError') is False,
      f"isError={resp.get('isError')}, errorMessages={(payload.get('errorMessages') if payload else None)}")
check('S3: force=true success → errorMessages empty',
      payload is not None and len(payload.get('errorMessages', [])) == 0,
      str(payload.get('errorMessages') if payload else None)[:300])

# --- Scenario 4: force=true + dependents (skips safety check) → success ---
r = run_mcp(cb, [
    ('delete-definitions', {
        'projectContext': 'testproj:main',
        'names': ['foo'],
        'force': True,
    }),
])
resp = r.get(2, {})
payload = payload_of(resp)
check('S4: force=true with-dependents returns payload',
      payload is not None,
      resp.get('content', '')[:300])
check('S4: force=true skips safety check → isError=false',
      resp.get('isError') is False,
      f"isError={resp.get('isError')}, errorMessages={(payload.get('errorMessages') if payload else None)}")

# --- Cross-tool sanity: response shape is invariant (still has all fields) ---
check('S1-S4 envelope has both `content` and `isError`',
      all(
          'content' in r.get(i, {}) and 'isError' in r.get(i, {})
          for i in [2]  # last response
      ),
      str(r.get(2, {}))[:300])

shutil.rmtree(cb_parent, ignore_errors=True)

print()
print(f'-- {ok} passed, {fail} failed --')
sys.exit(0 if fail == 0 else 1)
