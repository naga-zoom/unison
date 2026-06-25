#!/usr/bin/env python3
"""PR-63: delete-definitions returns useful response when blocked by dependents.

Pre-fix behavior (Task #14):
  delete-definitions with force=false on a definition with dependents
  returned UCM's `Output.NoUnisonFile` message:
    "There's nothing for me to add right now.
     Hint: I'm currently watching for definitions in .u files..."
  This is the *update*-command's error about a missing scratch file —
  emitted by handleDelete's `Cli.expectLatestFile` call when MCP mode
  hasn't loaded a scratch.u. Misleading and unactionable.

Post-fix behavior:
  The MCP layer prepends a virtual-source `UnisonFileChanged` event
  before `DeleteI False`, so UCM's `expectLatestFile` finds the virtual
  source and writes the dependents listing to it. The MCP response then
  carries:
    - errorMessages: real "couldn't complete the delete, dependents in use"
      message (UCM's `Output.DeleteFailure`)
    - sourceCodeUpdates: the actual source of the blocking dependents
  Caller now knows what's blocking the delete and can act on it.

Repro:
  - testproj with foo + bar (bar = foo, depends on foo)
  - delete-definitions force=false on foo → must NOT return NoUnisonFile
  - sourceCodeUpdates must include 'bar' (the dependent)
  - errorMessages must mention 'couldn't complete the delete' (not 'add')
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


def parse_payload(content):
    """Tool content is a JSON-encoded CliOutput; unpack it."""
    try:
        return json.loads(content)
    except (json.JSONDecodeError, TypeError):
        return None


cb_parent = tempfile.mkdtemp(prefix='ucm-pr63-')
cb = os.path.join(cb_parent, 'codebase')
print(f'-- PR-63 delete-deps-message smoke (codebase={cb}) --')

# Bootstrap: testproj with foo + bar (bar depends on foo)
scratch = os.path.join(cb_parent, 'scratch.u')
with open(scratch, 'w') as f:
    f.write('foo : Nat\nfoo = 1\n\nbar : Nat\nbar = foo\n')
out = run_cli(cb, cb_parent, [
    'project.create testproj',
    'switch testproj/main',
    'load scratch.u',
    'add',
], create=True)
check('bootstrap testproj with foo + bar (bar depends on foo)',
      '+ bar' in out and '+ foo' in out,
      out[-400:])
os.remove(scratch)

# --- THE REPRO: delete-definitions force=false on foo (bar depends on foo) ---
r = run_mcp(cb, [
    ('delete-definitions', {
        'projectContext': 'testproj:main',
        'names': ['foo'],
        # force omitted → defaults to false → triggers the dependents path
    }),
])
resp = r.get(2, {})
payload = parse_payload(resp.get('content', ''))
check('delete-definitions returns a structured payload',
      payload is not None,
      resp.get('content', '')[:300])

if payload is not None:
    em = ' '.join(payload.get('errorMessages', []))
    om = ' '.join(payload.get('outputMessages', []))
    scu = payload.get('sourceCodeUpdates', [])

    # Negative: must NOT return the misleading "nothing to add" message.
    check('response does NOT contain "nothing for me to add" (bug fix)',
          'nothing for me to add' not in (em + om).lower(),
          (em + om)[:400])

    # Positive: must mention the actual blocking reason.
    check('errorMessages mentions delete-blocked (not update)',
          "couldn't complete the delete" in em.lower(),
          em[:400])

    # Positive: dependents must surface in sourceCodeUpdates.
    check('sourceCodeUpdates is non-empty (dependents captured)',
          len(scu) > 0,
          str(scu)[:300])
    check('sourceCodeUpdates contains the dependent (bar)',
          any('bar' in s for s in scu),
          str(scu)[:300])
    check('sourceCodeUpdates references the deleted target (foo)',
          any('foo' in s for s in scu),
          str(scu)[:300])

# --- Regression check: force=true still works for dependents-blocked targets ---
r = run_mcp(cb, [
    ('delete-definitions', {
        'projectContext': 'testproj:main',
        'names': ['foo'],
        'force': True,
    }),
])
resp = r.get(2, {})
payload = parse_payload(resp.get('content', ''))
check('force=true delete succeeds even with dependents',
      payload is not None and "couldn't complete the delete" not in
        ' '.join(payload.get('errorMessages', []) if payload else []).lower(),
      str(payload)[:400] if payload else resp.get('content', '')[:300])

# --- Regression check: delete on a def with NO dependents succeeds without noise ---
# Set up a fresh isolated def.
scratch = os.path.join(cb_parent, 'scratch.u')
with open(scratch, 'w') as f:
    f.write('baz : Nat\nbaz = 42\n')
out = run_cli(cb, cb_parent, [
    'switch testproj/main',
    'load scratch.u',
    'add',
])
os.remove(scratch)

r = run_mcp(cb, [
    ('delete-definitions', {
        'projectContext': 'testproj:main',
        'names': ['baz'],
        # force omitted; no dependents so should just succeed
    }),
])
resp = r.get(2, {})
payload = parse_payload(resp.get('content', ''))
check('delete of dependent-free def succeeds (no regression)',
      payload is not None and "couldn't complete the delete" not in
        ' '.join(payload.get('errorMessages', []) if payload else []).lower(),
      str(payload)[:400] if payload else resp.get('content', '')[:300])

shutil.rmtree(cb_parent, ignore_errors=True)

print()
print(f'-- {ok} passed, {fail} failed --')
sys.exit(0 if fail == 0 else 1)
