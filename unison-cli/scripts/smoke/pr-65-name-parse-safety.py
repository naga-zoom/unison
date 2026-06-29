#!/usr/bin/env python3
"""PR-65: MCP tools reject invalid identifiers cleanly (no JSON-RPC -32603).

Symptom (discovered while probing PR-64): MCP/Types.hs FromJSON instances
call `Name.unsafeParseText`, `NameSegment.unsafeParseText`, and
`Path.unsafeParseText[']` directly. These panic via `HasCallStack` on
invalid identifiers like "foo-bar" (hyphens parse as binary operators,
not name chars), surfacing as JSON-RPC `code: -32603` (server crash)
instead of a typed MCP error.

Pre-fix: caller gets:
    {"error": {"code": -32603, "message": "1:5:\n  | ... unexpected '-'\n
                                           CallStack ... unsafeParseText"}}
That's a server-side crash trace, not an MCP-level "invalid argument"
error. Callers (especially LLM agents that occasionally synthesize
identifier strings) can't recover with a structured error message.

Post-fix: caller gets:
    {"result": {"isError": true, "content": "Failed to parse arguments
                                             for tool '<name>': invalid
                                             name: foo-bar — names must be
                                             valid Unison identifiers..."}}
Same envelope shape as other parse failures. Callers can retry with
sanitized input.

Tools under test (each takes a Name/NameSegment/Path arg):
  T1 delete-definitions  — names
  T2 compile             — mainFunctionName
  T3 rename-definition   — oldName + newNameSegment
  T4 delete-namespace    — namespaceName
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


def run_cli(cb, cwd, lines, create=False, timeout=60):
    stdin = '\n'.join(lines + ['quit', '']).encode()
    flag = '-C' if create else '-c'
    return subprocess.run([UCM, flag, cb], input=stdin, capture_output=True,
                          timeout=timeout, cwd=cwd).stdout.decode(errors='replace')


def run_mcp_raw(cb, calls, create=False, timeout=60):
    """Returns the FULL JSON-RPC envelopes (incl. server-side errors)."""
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
    by_id = {}
    for line in r.stdout.splitlines():
        try:
            o = json.loads(line)
            rid = o.get('id')
            if rid and rid >= 2:
                by_id[rid] = o
        except json.JSONDecodeError:
            pass
    return by_id


def call_is_jsonrpc_server_error(envelope):
    """Returns True if the envelope is a JSON-RPC server-side crash."""
    return 'error' in envelope and envelope.get('error', {}).get('code') is not None


def call_is_mcp_typed_error(envelope):
    """Returns True if the envelope is a normal MCP result with isError=true."""
    r = envelope.get('result', {})
    return r and r.get('isError') is True


ok = 0
fail = 0


def check(label, cond, detail=''):
    global ok, fail
    if cond:
        print(f'  PASS  {label}')
        ok += 1
    else:
        print(f'  FAIL  {label}    {detail[:500]}')
        fail += 1


cb_parent = tempfile.mkdtemp(prefix='ucm-pr65-')
cb = os.path.join(cb_parent, 'codebase')
print(f'-- PR-65 name-parse safety smoke (codebase={cb}) --')

# Bootstrap a project (mostly empty — we don't need real defs since the
# parse failure fires BEFORE handler lookup).
run_cli(cb, cb_parent, [
    'project.create testproj',
    'switch testproj/main',
], create=True)

# Invalid identifier — hyphen is a binary operator, not a name char.
BAD = 'definitely-not-a-real-name'

# --- T1: delete-definitions with hyphenated name ---
envs = run_mcp_raw(cb, [
    ('delete-definitions', {
        'projectContext': 'testproj:main',
        'names': [BAD],
    }),
])
env = envs.get(2, {})
check('T1: delete-definitions does NOT JSON-RPC-crash on invalid name',
      not call_is_jsonrpc_server_error(env),
      json.dumps(env)[:500])
check('T1: delete-definitions returns MCP typed error (isError=true)',
      call_is_mcp_typed_error(env),
      json.dumps(env)[:500])

# --- T2: compile with hyphenated mainFunctionName ---
envs = run_mcp_raw(cb, [
    ('compile', {
        'projectContext': 'testproj:main',
        'mainFunctionName': BAD,
        'outputPath': '/tmp/out.uc',
    }),
])
env = envs.get(2, {})
check('T2: compile does NOT JSON-RPC-crash on invalid name',
      not call_is_jsonrpc_server_error(env),
      json.dumps(env)[:500])
check('T2: compile returns MCP typed error (isError=true)',
      call_is_mcp_typed_error(env),
      json.dumps(env)[:500])

# --- T3a: rename-definition with hyphenated oldName ---
envs = run_mcp_raw(cb, [
    ('rename-definition', {
        'projectContext': 'testproj:main',
        'oldName': BAD,
        'newNameSegment': 'valid',
    }),
])
env = envs.get(2, {})
check('T3a: rename-definition does NOT crash on invalid oldName',
      not call_is_jsonrpc_server_error(env),
      json.dumps(env)[:500])
check('T3a: rename-definition returns MCP typed error',
      call_is_mcp_typed_error(env),
      json.dumps(env)[:500])

# --- T3b: rename-definition with hyphenated newNameSegment ---
envs = run_mcp_raw(cb, [
    ('rename-definition', {
        'projectContext': 'testproj:main',
        'oldName': 'valid',
        'newNameSegment': BAD,
    }),
])
env = envs.get(2, {})
check('T3b: rename-definition does NOT crash on invalid newNameSegment',
      not call_is_jsonrpc_server_error(env),
      json.dumps(env)[:500])
check('T3b: rename-definition returns MCP typed error',
      call_is_mcp_typed_error(env),
      json.dumps(env)[:500])

# --- T4: delete-namespace with hyphenated namespaceName ---
envs = run_mcp_raw(cb, [
    ('delete-namespace', {
        'projectContext': 'testproj:main',
        'namespaceName': BAD,
    }),
])
env = envs.get(2, {})
check('T4: delete-namespace does NOT crash on invalid namespaceName',
      not call_is_jsonrpc_server_error(env),
      json.dumps(env)[:500])
check('T4: delete-namespace returns MCP typed error',
      call_is_mcp_typed_error(env),
      json.dumps(env)[:500])

# --- Sanity: valid identifiers still work (no regression) ---
# Add a term so we can rename it cleanly.
scratch = os.path.join(cb_parent, 'scratch.u')
with open(scratch, 'w') as f:
    f.write('myValue : Nat\nmyValue = 7\n')
run_cli(cb, cb_parent, [
    'switch testproj/main',
    'load scratch.u',
    'add',
])
os.remove(scratch)

envs = run_mcp_raw(cb, [
    ('rename-definition', {
        'projectContext': 'testproj:main',
        'oldName': 'myValue',
        'newNameSegment': 'renamedValue',
    }),
])
env = envs.get(2, {})
check('Sanity: rename-definition with valid identifiers succeeds',
      not call_is_jsonrpc_server_error(env) and not call_is_mcp_typed_error(env),
      json.dumps(env)[:500])

shutil.rmtree(cb_parent, ignore_errors=True)

print()
print(f'-- {ok} passed, {fail} failed --')
sys.exit(0 if fail == 0 else 1)
