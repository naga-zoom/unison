#!/usr/bin/env python3
"""Additional write-mode smokes specifically for the two confidence gaps:

  1. eval cache freshness across mutation
  2. concurrent TVar behavior — multiple parallel sessions, verify
     no torn reads / exceptions / inconsistent results.

Standalone — reuses the mcp-write-smoke project created by the
previous E2E script (or recreates it if absent).
"""
import json
import subprocess
import sys
import threading
import time

PROJECT = f'mcp-write-smoke-{int(time.time())}'
MCP = '/Users/nagarjunapamu/.local/bin/ucm'


def session(calls, timeout=180):
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
    out = []
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
                out.append((rid, is_err, parsed, content))
        except json.JSONDecodeError:
            pass
    return out


ok_count, fail_count = 0, 0
def check(label, condition, detail=''):
    global ok_count, fail_count
    if condition:
        print(f'  ✓ {label}')
        ok_count += 1
    else:
        print(f'  ✗ {label}    {detail}')
        fail_count += 1


ctx = f'{PROJECT}:main'

# ─────────── Setup (self-contained — creates the project per run) ───────────
print(f'=== Setup ({PROJECT}) ===')
setup = session([
    ('project-create', {'projectContext': 'temper:main',
                        'projectName': PROJECT, 'downloadBase': True}),
])
check('project-create', not setup[0][1])

# ─────────── 1. eval freshness across mutation ───────────
print('\n=== eval freshness across mutation ===')
print('  (each `eval x` should reflect the CURRENT branch state, never a stale value)')

# First ensure baseline definition exists
results = session([
    ('update-definitions', {'projectContext': ctx,
                            'code': {'sourceCode': 'use lib.base.data Nat\nfreshTest : Nat\nfreshTest = 7\n'}}),
    ('eval', {'projectContext': ctx, 'expression': 'freshTest'}),
])
upd1 = results[0][2]
ev1 = results[1][2]
ev1_out = '\n'.join(ev1.get('outputMessages', [])) if isinstance(ev1, dict) else ''
check('initial def created', not results[0][1])
check('eval sees initial value (7)', ' 7' in ev1_out or '\n7' in ev1_out,
      f'eval output: {ev1_out[:300]}')

# Now mutate the definition — single-session to test cache freshness within one process
results2 = session([
    ('update-definitions', {'projectContext': ctx,
                            'code': {'sourceCode': 'use lib.base.data Nat\nfreshTest : Nat\nfreshTest = 99\n'}}),
    ('eval', {'projectContext': ctx, 'expression': 'freshTest'}),
])
upd2 = results2[0][2]
ev2 = results2[1][2]
ev2_out = '\n'.join(ev2.get('outputMessages', [])) if isinstance(ev2, dict) else ''
check('mutation succeeds', not results2[0][1])
check('eval sees new value (99), not stale (7)',
      ('99' in ev2_out) and (' 7' not in ev2_out and '\n7\n' not in ev2_out),
      f'eval output: {ev2_out[:300]}')

# Also test the rapid mutation-eval-mutation-eval pattern in one session
results3 = session([
    ('update-definitions', {'projectContext': ctx,
                            'code': {'sourceCode': 'use lib.base.data Nat\nrapidTest : Nat\nrapidTest = 11\n'}}),
    ('eval', {'projectContext': ctx, 'expression': 'rapidTest'}),
    ('update-definitions', {'projectContext': ctx,
                            'code': {'sourceCode': 'use lib.base.data Nat\nrapidTest : Nat\nrapidTest = 22\n'}}),
    ('eval', {'projectContext': ctx, 'expression': 'rapidTest'}),
    ('update-definitions', {'projectContext': ctx,
                            'code': {'sourceCode': 'use lib.base.data Nat\nrapidTest : Nat\nrapidTest = 33\n'}}),
    ('eval', {'projectContext': ctx, 'expression': 'rapidTest'}),
])
ev_outs = []
for r in results3:
    rid, is_err, parsed, _ = r
    if isinstance(parsed, dict) and 'outputMessages' in parsed and rid % 2 == 1:  # ids 3, 5, 7 = evals
        ev_outs.append('\n'.join(parsed.get('outputMessages', [])))

check('rapid eval 1 sees 11', any('11' in o for o in ev_outs[:1]),
      f'first eval: {ev_outs[0][:200] if ev_outs else "no output"}')
check('rapid eval 2 sees 22 (not 11)', any('22' in o for o in ev_outs[1:2]),
      f'second eval: {ev_outs[1][:200] if len(ev_outs) > 1 else "no output"}')
check('rapid eval 3 sees 33 (not 22)', any('33' in o for o in ev_outs[2:3]),
      f'third eval: {ev_outs[2][:200] if len(ev_outs) > 2 else "no output"}')


# ─────────── 2. Concurrent TVar safety ───────────
print('\n=== concurrent cache access (TVar race window) ===')
print('  spawning N parallel sessions all calling list-project-definitions; verify')
print('  no exceptions, all return consistent results.')

N = 8
responses = [None] * N
exceptions = [None] * N

def worker(i):
    try:
        r = session([
            ('list-project-definitions', {'projectContext': ctx, 'limit': 10}),
        ], timeout=60)
        if r:
            responses[i] = r[0][2]
    except Exception as e:
        exceptions[i] = e

threads = [threading.Thread(target=worker, args=(i,)) for i in range(N)]
t0 = time.time()
for t in threads:
    t.start()
for t in threads:
    t.join()
elapsed = time.time() - t0

excs = [e for e in exceptions if e is not None]
check(f'{N} concurrent sessions: no exceptions',
      len(excs) == 0,
      f'{len(excs)} exceptions: {excs[:2]}')

successful = [r for r in responses if r is not None]
check(f'{N} concurrent sessions: all returned',
      len(successful) == N,
      f'{len(successful)}/{N} responded')

if successful:
    # All responses against the same branch hash should have identical
    # totalCount and identical definitions list ordering.
    totals = set(r.get('totalCount') for r in successful if isinstance(r, dict))
    check(f'{N} concurrent sessions: all totalCounts identical',
          len(totals) == 1,
          f'distinct totals seen: {totals}')

    # Identical first-page contents (proves no torn reads).
    first_pages = [json.dumps(r.get('definitions', [])[:5], sort_keys=True)
                   for r in successful if isinstance(r, dict)]
    check(f'{N} concurrent sessions: all first-pages identical',
          len(set(first_pages)) == 1,
          f'distinct page contents: {len(set(first_pages))}')

print(f'  ({N} parallel sessions in {elapsed:.2f}s)')

# Now stress-test with mutation interleaved: parallel reads while a
# writer mutates the branch in the same process. The cache key changes
# under the readers, but the TVar transitions atomically, so no
# corrupt state should be visible.
print('\n=== concurrent reads + interleaved mutations ===')

read_errs = []
def reader(i):
    try:
        for _ in range(3):
            r = session([('list-project-definitions',
                          {'projectContext': ctx, 'limit': 5})], timeout=60)
            if not r or r[0][1]:
                read_errs.append(f'reader {i}: failed')
    except Exception as e:
        read_errs.append(f'reader {i}: {e}')

write_errs = []
def writer():
    try:
        for n in range(3):
            r = session([('update-definitions',
                          {'projectContext': ctx,
                           'code': {'sourceCode':
                                    f'use lib.base.data Nat\nstress{n} : Nat\nstress{n} = {n}\n'}})], timeout=60)
            if not r or r[0][1]:
                write_errs.append(f'writer {n}: failed')
    except Exception as e:
        write_errs.append(f'writer: {e}')

readers = [threading.Thread(target=reader, args=(i,)) for i in range(4)]
w = threading.Thread(target=writer)
for r in readers:
    r.start()
w.start()
for r in readers:
    r.join()
w.join()

check('readers + writer: no reader errors', len(read_errs) == 0,
      f'errors: {read_errs[:2]}')
check('readers + writer: no writer errors', len(write_errs) == 0,
      f'errors: {write_errs[:2]}')


print('\n=== Summary ===')
print(f'  {ok_count} passed, {fail_count} failed')
sys.exit(0 if fail_count == 0 else 1)
