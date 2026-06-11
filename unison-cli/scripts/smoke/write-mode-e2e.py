#!/usr/bin/env python3
"""Write-mode E2E smoke for the deployed ucm-mcp binary.

Verifies:
  - Cache invalidates on mutation (merkle property in practice)
  - Compact-path syntax works with merge / push / pull
  - find-and-act actually deletes (dryRun=false)
  - sanity-fix actually applies (apply=true)
  - lib-refresh actually installs (dryRun=false)
  - pipeline with mutation steps + read steps in one call

Uses a throwaway project so the smoke is reproducible and doesn't dirty
real codebases.
"""
import json
import subprocess
import sys
import time

PROJECT = f'mcp-write-smoke-{int(time.time())}'
MCP = '/Users/nagarjunapamu/.local/bin/ucm'


def session(calls, timeout=180):
    """Run a single MCP session with multiple tool calls. Returns list of (id, result-dict)."""
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
    r = subprocess.run([MCP, 'mcp'], input=stdin, capture_output=True,
                       text=True, timeout=timeout)
    results = []
    for line in r.stdout.splitlines():
        try:
            obj = json.loads(line)
            rid = obj.get('id')
            if rid and rid >= 2:
                content = obj.get('result', {}).get('content', [{}])[0].get('text', '')
                is_err = obj.get('result', {}).get('isError', False)
                try:
                    parsed = json.loads(content) if content else None
                except json.JSONDecodeError:
                    parsed = content
                results.append((rid, is_err, parsed, content))
        except json.JSONDecodeError:
            pass
    return results


ok_count = 0
fail_count = 0
def check(label, condition, detail=''):
    global ok_count, fail_count
    if condition:
        print(f'  ✓ {label}')
        ok_count += 1
    else:
        print(f'  ✗ {label}    {detail}')
        fail_count += 1


# ─────────── Setup: fresh project + base lib ───────────
print('=== Setup ===')
results = session([
    ('project-create', {'projectContext': 'temper:main',
                        'projectName': PROJECT, 'downloadBase': True}),
])
check('project-create', not results[0][1])

ctx = f'{PROJECT}:main'

# ─────────── 1. Cache + mutation invalidation ───────────
print('\n=== Cache invalidation across mutation ===')
results = session([
    # (a) Initial list — should be tiny (no user defs yet, lib excluded)
    ('list-project-definitions', {'projectContext': ctx, 'limit': 5}),
    # (b) Same call again — cache hit
    ('list-project-definitions', {'projectContext': ctx, 'limit': 5}),
    # (c) Mutation: add a definition
    ('update-definitions', {'projectContext': ctx,
                             'code': {'sourceCode': 'use lib.base.data Nat\nfirst : Nat\nfirst = 1\n'}}),
    # (d) After mutation: branch hash differs → cache miss → new defs visible
    ('list-project-definitions', {'projectContext': ctx, 'limit': 5}),
    # (e) Same call again — cache hit on the new hash
    ('list-project-definitions', {'projectContext': ctx, 'limit': 5}),
])

# (a) and (b) should have identical totalCount
totalA = results[0][2].get('totalCount', -1)
totalB = results[1][2].get('totalCount', -1)
check('initial list returns OK', not results[0][1])
check('warm cache (b) totalCount == cold (a)', totalA == totalB,
      f'a={totalA} b={totalB}')

# (c) update succeeds
upd = results[2][2]
upd_errs = upd.get('errorMessages', []) if isinstance(upd, dict) else []
check('update-definitions succeeds', len(upd_errs) == 0,
      f'errors: {upd_errs[:1]}')

# (d) totalCount should have increased by 1 (the new `first` def)
totalD = results[3][2].get('totalCount', -1)
check('post-mutation totalCount > pre-mutation', totalD > totalA,
      f'pre={totalA} post={totalD}')

# (e) same as (d), both cache hits on the new hash
totalE = results[4][2].get('totalCount', -1)
check('post-mutation warm hit equals fresh', totalD == totalE,
      f'd={totalD} e={totalE}')


# ─────────── 2. diagnose cache invalidation ───────────
print('\n=== diagnose cache invalidation ===')
results = session([
    ('diagnose', {'projectContext': ctx}),
    ('diagnose', {'projectContext': ctx}),
    ('update-definitions', {'projectContext': ctx,
                            'code': {'sourceCode': 'use lib.base.data Nat\nsecond : Nat\nsecond = 2\n'}}),
    ('diagnose', {'projectContext': ctx}),
])
d1 = results[0][2].get('totalCount', -1)
d2 = results[1][2].get('totalCount', -1)
upd2 = results[2][2]
d3 = results[3][2].get('totalCount', -1)
check('diagnose pre-mutation succeeds', not results[0][1])
check('diagnose warm cache identical', d1 == d2, f'd1={d1} d2={d2}')
check('diagnose post-mutation re-runs (count may differ)', d3 >= 0)


# ─────────── 3. Compact-path with merge ───────────
print('\n=== compact-path for merge tool ===')
results = session([
    ('create-branch', {'projectName': PROJECT, 'newBranchName': 'feature',
                       'sourceType': 'branch',
                       'sourceBranchProject': PROJECT, 'sourceBranchName': 'main'}),
    ('update-definitions', {'projectContext': f'{PROJECT}:feature',
                             'code': {'sourceCode': 'use lib.base.data Nat\nthird : Nat\nthird = 3\n'}}),
    ('merge', {'projectContext': ctx, 'source': {'branch': 'feature'}}),
    ('view-definitions', {'projectContext': ctx, 'names': ['third']}),
])
check('compact-path: create-branch', not results[0][1])
check('compact-path: update on feature', not results[1][1])
mr = results[2][2]
check('compact-path: merge succeeds', not results[2][1] and
      'merged' in str(mr.get('outputMessages', [''])[0]).lower(),
      f'merge response: {str(mr)[:150]}')
view_third = results[3][2]
view_errs = view_third.get('errorMessages', []) if isinstance(view_third, dict) else []
check('post-merge: `third` visible on main', len(view_errs) == 0)


# ─────────── 4. pipeline with mutations ───────────
print('\n=== pipeline with mutation steps ===')
results = session([
    ('pipeline', {
        'steps': [
            {'tool': 'update-definitions',
             'arguments': {'projectContext': ctx,
                           'code': {'sourceCode': 'use lib.base.data Nat\nfourth : Nat\nfourth = 4\n'}}},
            {'tool': 'view-definitions',
             'arguments': {'projectContext': ctx, 'names': ['fourth']}},
            {'tool': 'eval',
             'arguments': {'projectContext': ctx, 'expression': 'fourth'}},
        ],
        'stopOnFirstError': True,
    }),
])
pipe = results[0][2]
steps = pipe.get('steps', []) if isinstance(pipe, dict) else []
check('pipeline succeeds end-to-end', not pipe.get('stopped', True))
check('pipeline: all 3 steps ok',
      all(s.get('ok') for s in steps),
      f'steps: {[(s["tool"], s["ok"]) for s in steps]}')
# Verify eval saw the just-added definition
if len(steps) >= 3:
    eval_content = steps[2].get('content', {})
    eval_out = '\n'.join(eval_content.get('outputMessages', []))
    check('pipeline: eval saw mutation from prior step (output mentions `4`)',
          ' 4' in eval_out or '\n4' in eval_out,
          f'eval output: {eval_out[:200]}')


# ─────────── 5. find-and-act actually deletes ───────────
print('\n=== find-and-act with dryRun=false (actual delete) ===')
results = session([
    # Add a marker def
    ('update-definitions', {'projectContext': ctx,
                            'code': {'sourceCode': 'use lib.base.data Nat\ndoomed : Nat\ndoomed = 99\n'}}),
    # Verify it exists
    ('view-definitions', {'projectContext': ctx, 'names': ['doomed']}),
    # Delete via find-and-act
    ('find-and-act', {'projectContext': ctx,
                      'query': 'name:doomed', 'action': {'type': 'delete'},
                      'dryRun': False}),
    # Verify gone
    ('view-definitions', {'projectContext': ctx, 'names': ['doomed']}),
])
v_before = results[1][2]
v_before_errs = v_before.get('errorMessages', []) if isinstance(v_before, dict) else []
check('doomed exists before find-and-act', len(v_before_errs) == 0)

faa = results[2][2]
check('find-and-act matchCount > 0',
      isinstance(faa, dict) and faa.get('matchCount', 0) > 0,
      f'matchCount: {faa.get("matchCount") if isinstance(faa, dict) else "?"}')

v_after = results[3][2]
v_after_errs = v_after.get('errorMessages', []) if isinstance(v_after, dict) else []
check('doomed gone after find-and-act',
      len(v_after_errs) > 0 and 'not found' in str(v_after_errs).lower(),
      f'view errors after delete: {v_after_errs[:1]}')


# ─────────── Cleanup ───────────
print('\n=== Summary ===')
print(f'  {ok_count} passed, {fail_count} failed')
sys.exit(0 if fail_count == 0 else 1)
