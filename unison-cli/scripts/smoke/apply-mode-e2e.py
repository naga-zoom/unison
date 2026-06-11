#!/usr/bin/env python3
"""Apply-mode smokes for the remaining write paths.

Targets the 5 ⏸️ items from the audit:
  1. release with dryRun=false (creates a release branch)
  2. cross-project-move with dryRun=false (across two sample projects)
  3. lib-refresh non-dry-run (re-installs base; idempotent or clear failure)
  4. sanity-fix with apply=true (auto-moves misplaced ctors)
  5. push/pull in compact-path form (parse + run; expect Share auth /
     missing-remote errors but no syntactic rejection)

Uses throwaway projects so the smoke is reproducible.
"""
import json
import subprocess
import sys
import time

MCP = '/Users/nagarjunapamu/.local/bin/ucm'
PROJ_A = f'mcp-apply-smoke-{int(time.time())}-a'
PROJ_B = f'mcp-apply-smoke-{int(time.time())}-b'


def session(calls, timeout=300):
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
def check(label, cond, detail=''):
    global ok_count, fail_count
    if cond:
        print(f'  ✓ {label}')
        ok_count += 1
    else:
        print(f'  ✗ {label}    {detail}')
        fail_count += 1


# ─────────── Setup: two sample projects with a Nat def in each ───────────
print('=== Setup ===')
setup = session([
    ('project-create', {'projectContext': 'temper:main',
                        'projectName': PROJ_A, 'downloadBase': True}),
    ('update-definitions',
     {'projectContext': f'{PROJ_A}:main',
      'code': {'sourceCode': 'use lib.base.data Nat\nseed : Nat\nseed = 100\n'}}),
    ('project-create', {'projectContext': 'temper:main',
                        'projectName': PROJ_B, 'downloadBase': True}),
])
check('proj A created', not setup[0][1])
check('def `seed` added to A', not setup[1][1])
check('proj B created', not setup[2][1])


# ─────────── 1. release with dryRun=false ───────────
print('\n=== release apply mode ===')
res = session([
    ('release', {'projectContext': f'{PROJ_A}:main', 'version': '0.1.0', 'dryRun': False}),
    ('list-project-branches', {'projectName': PROJ_A}),
])
rel = res[0][2]
check('release dryRun=false returns OK',
      isinstance(rel, dict) and rel.get('ok') is True,
      f'release response: {str(rel)[:300]}')
check('release.branchCreated == releases/drafts/0.1.0',
      isinstance(rel, dict) and rel.get('branchCreated') == 'releases/drafts/0.1.0',
      f'branchCreated: {rel.get("branchCreated") if isinstance(rel, dict) else "?"}')
branches = res[1][2]
branch_names = [b['name'] for b in (branches.get('branches', []) if isinstance(branches, dict) else [])]
check('release branch visible in list-project-branches',
      'releases/drafts/0.1.0' in branch_names,
      f'branches: {branch_names}')


# ─────────── 2. cross-project-move apply ───────────
print('\n=== cross-project-move apply mode ===')
res = session([
    # Move `seed` from A → B
    ('cross-project-move', {
        'srcContext': f'{PROJ_A}:main',
        'srcName': 'seed',
        'destContext': f'{PROJ_B}:main',
        'destName': 'imported',
        'dryRun': False,
    }),
    # Verify imported lives in B
    ('view-definitions', {'projectContext': f'{PROJ_B}:main', 'names': ['imported']}),
    # Verify seed is gone from A
    ('view-definitions', {'projectContext': f'{PROJ_A}:main', 'names': ['seed']}),
])
cpm = res[0][2]
check('cross-project-move returns committed or rolled-back',
      isinstance(cpm, dict) and cpm.get('status') in ('committed', 'rolled-back'),
      f'cpm response: {str(cpm)[:200]}')

v_dest = res[1][2]
v_dest_errs = v_dest.get('errorMessages', []) if isinstance(v_dest, dict) else []
check('imported visible in proj B',
      len(v_dest_errs) == 0,
      f'errors viewing B.imported: {v_dest_errs[:1]}')

v_src = res[2][2]
v_src_errs = v_src.get('errorMessages', []) if isinstance(v_src, dict) else []
if cpm.get('status') == 'committed':
    check('seed gone from proj A',
          len(v_src_errs) > 0 and 'not found' in str(v_src_errs).lower(),
          f'A.seed view: {v_src_errs[:1]}')
else:
    check('rolled-back: seed remains in proj A',
          len(v_src_errs) == 0,
          f'rolled back; A.seed: {v_src_errs}')


# ─────────── 3. lib-refresh non-dry-run ───────────
print('\n=== lib-refresh apply (re-install @unison/base) ===')
res = session([
    ('lib-refresh', {
        'projectContext': f'{PROJ_A}:main',
        'libProjectName': '@unison/base',
        'dryRun': False,
    }),
])
lr = res[0][2]
check('lib-refresh apply returns structured response',
      isinstance(lr, dict) and 'install' in lr and 'plan' in lr,
      f'lib-refresh keys: {list(lr.keys()) if isinstance(lr, dict) else "?"}')
inst = lr.get('install') if isinstance(lr, dict) else None
check('lib-refresh attempted the install (install != null)',
      inst is not None,
      f'install: {inst}')


# ─────────── 4. sanity-fix with apply=true ───────────
print('\n=== sanity-fix apply mode ===')
res = session([
    ('sanity-fix', {'projectContext': f'{PROJ_A}:main', 'apply': True}),
])
sf = res[0][2]
check('sanity-fix returns structured response',
      isinstance(sf, dict) and 'applied' in sf and 'suggested' in sf,
      f'sf keys: {list(sf.keys()) if isinstance(sf, dict) else "?"}')
# Applied list may be empty (proj A has no misplaced ctors) — that's fine,
# proves the apply path runs without exception.
applied = sf.get('applied', []) if isinstance(sf, dict) else []
print(f'    applied {len(applied)} fixes (may be 0 if no misplaced ctors)')
check('sanity-fix apply runs without isError', not res[0][1])


# ─────────── 5. push/pull with compact-path projectContext ───────────
print('\n=== push/pull compact-path syntax (parse + run; Share interaction may fail) ===')
# We don't actually want to push to a real Share remote, but we DO want
# to verify the compact path syntax is accepted by FromJSON. The tool's
# downstream Share auth / remote-not-set error is the expected outcome.
res = session([
    ('push', {'projectContext': f'{PROJ_A}:main'}),
    ('pull', {'projectContext': f'{PROJ_A}:main',
              'source': {'project': '@unison/base', 'branch': 'main'}}),
])

# We expect non-failure to PARSE, but the Share call may produce errors.
push_resp = res[0][2]
push_is_err = res[0][1]
push_str = str(push_resp)[:200] if push_resp else ''
check('push: compact projectContext accepted by FromJSON',
      'Failed to parse arguments' not in push_str,
      f'push response: {push_str}')

pull_resp = res[1][2]
pull_str = str(pull_resp)[:200] if pull_resp else ''
check('pull: compact projectContext accepted by FromJSON',
      'Failed to parse arguments' not in pull_str,
      f'pull response: {pull_str}')


print('\n=== Summary ===')
print(f'  {ok_count} passed, {fail_count} failed')
sys.exit(0 if fail_count == 0 else 1)
