# MCP Spec Extensions: Session State + Cached Resources

A proposal to extend the Model Context Protocol (modelcontextprotocol/specification) with two related capabilities that have produced the largest per-call token cost we've measured in production agent traffic against the Unison MCP surface.

**Status:** draft. Intended to seed a thread on modelcontextprotocol/specification + a reference implementation against ucm-mcp.

---

## Problem

Two distinct but related inefficiencies appear in long-lived agent → MCP-server sessions:

### 1. Tool args duplicate session state on every call

Every `tools/call` ships the full args object. When the server is stateful by nature — a database connection, a working directory, a project/branch context, a tenant ID — every call must re-include those fields. They are not tied to the tool's *intent*; they are tied to the *session's* current setting.

Concrete measurement against ucm-mcp:

- Of ~50 tools in the surface, ~80% accept a `projectContext` arg.
- `projectContext` carries `{projectName, branchName}` — typically 40-80 bytes.
- An average 30-tool-call agent loop carries the same context ~24 times.
- In a typical multi-turn task we measured **~1.2KB of redundant context-repetition** per loop, ~6% of total tool-call token cost.

The redundancy is not just bandwidth. It's also a source of bugs: a fast-typing agent that sends `mainBranch` instead of `branchName` for one of N calls drifts off the intended scope.

### 2. Resource state is re-fetched across calls that don't change it

MCP today exposes `resources/list` and `resources/read` for static-ish data. Neither defines caching semantics. Tools that walk the same large structure repeatedly (a project branch's `deepTerms`, a code search index, a docs index) re-walk it on every read.

Concrete measurement:

- `list-project-definitions` on `assay` walks 18,039 terms × 427 types per call.
- A multi-turn agent loop that runs `find`, `diagnose`, then `list-project-definitions` each pays the same cost.
- Sequential repeats of the same logical fetch: **~30-60s of redundant codebase walking** per loop.

In an agent loop that runs N+1 read-style calls against the same branch, the server does N redundant scans of state that hasn't changed.

## Proposal

Two additions, both **opt-in** at the server level and **backwards-compatible**:

### Capability 1: Session State (`sessionState`)

Add a new capability advertised in `initialize.serverCapabilities`:

```json
{
  "capabilities": {
    "sessionState": {
      "schema": {
        "type": "object",
        "properties": {
          "projectContext": { ... },
          "tenantId":       { "type": "string" }
        }
      }
    }
  }
}
```

The server declares a JSON Schema for the state shape. The client maintains a mutable state object matching that schema and includes it in every subsequent `tools/call` under a reserved field:

```json
{
  "method": "tools/call",
  "params": {
    "name": "view-definitions",
    "arguments": {
      "names": ["List.map"]
    },
    "_session": {
      "projectContext": {
        "projectName": "@unison/base",
        "branchName": "main"
      }
    }
  }
}
```

Server-side semantics: when a tool's `inputSchema` declares a field that appears in `_session`, the server fills it from `_session` *unless* the call's `arguments` provides an explicit override.

A dedicated method lets the client mutate state without invoking a tool:

```json
{
  "method": "session/setState",
  "params": {
    "merge": { "projectContext": { "projectName": "loom", "branchName": "main" } }
  }
}
```

The server validates the merge against its declared schema and either applies it or returns a `400`-style error.

The server may also push state back:

```json
{
  "method": "notifications/session/stateChanged",
  "params": {
    "state": { "projectContext": { ... } }
  }
}
```

— useful when a tool (`project-rename`, `switch`) implicitly updates the agent's working scope.

### Capability 2: Cached Resources (`resourceCaching`)

Add a new capability:

```json
{
  "capabilities": {
    "resourceCaching": {
      "etag": true,
      "ifNoneMatch": true,
      "stateChangedNotifications": true
    }
  }
}
```

Extend the existing `resources/read` response to carry an opaque `etag`:

```json
{
  "result": {
    "contents": [ { "uri": "...", "text": "..." } ],
    "etag": "Wnf-2026-06-11T03:32:47Z-a3f2b1"
  }
}
```

Clients persist the `etag` and pass it back on subsequent reads:

```json
{
  "method": "resources/read",
  "params": {
    "uri": "ucm://project/temper/branch/main/deepTerms",
    "ifNoneMatch": "Wnf-2026-06-11T03:32:47Z-a3f2b1"
  }
}
```

When the resource is unchanged, the server returns a thin response with no `contents`:

```json
{ "result": { "notModified": true, "etag": "Wnf-2026-06-11T03:32:47Z-a3f2b1" } }
```

When changed, the server returns full `contents` with a new `etag`.

When a tool mutates resource state, the server emits:

```json
{
  "method": "notifications/resources/changed",
  "params": { "uris": ["ucm://project/temper/branch/main/deepTerms"] }
}
```

— so clients can preemptively evict cached copies (or just let the next 304 do it).

## Where the tokens go

Per-call savings on a typical agent loop against ucm-mcp (50 tools, 30 calls):

| Source of bloat | Before | After |
|---|---|---|
| `projectContext` repetition (24 calls × 60 bytes) | 1.4 KB | 0 KB |
| Resource re-fetch (3× `list-project-defs` × 80 KB JSON) | 240 KB | 80 KB (1 fetch, 2 × 304s of ~120 bytes) |
| **Net token cost per loop** | **241 KB** | **80 KB** (≈3× reduction) |

In time:

| | Before | After |
|---|---|---|
| Cold reads | identical | identical |
| Warm reads | identical | ~50 ms (etag check + 304) vs. ~30 s (full walk) |

## Backwards compatibility

Both extensions are **opt-in via capability negotiation**. A client that doesn't advertise support for either capability gets exactly today's behavior. A server that doesn't advertise `sessionState` or `resourceCaching` likewise behaves identically to today.

Concretely:

- `_session` in a request to a non-`sessionState` server is silently ignored (treated as an unknown extension field by the server's argument parser).
- `ifNoneMatch` against a non-`resourceCaching` server returns the full contents (no `notModified` short-circuit).

There is no fork in the wire format — both extensions add fields without changing the meaning of existing ones.

## Reference implementation sketch

Server-side, in the ucm-mcp Haskell impl, both fit in ~150 LOC:

```haskell
-- session state
data Session = Session
  { state :: TVar Value           -- the user's session-scoped state
  , schema :: Value               -- the JSON schema we declared at init
  }

-- on tools/call: read _session, merge under arguments, validate, dispatch
handleToolCall :: Request -> MCP Response
handleToolCall req = do
  sess <- asks session
  s <- readTVarIO sess.state
  let args = req.params.arguments `merge` (s `restrictTo` req.params.tool.schemaFields)
  invoke req.params.tool args

-- resources/read with etag
handleResourceRead :: Request -> MCP Response
handleResourceRead req = do
  let uri = req.params.uri
  current <- computeEtag uri
  case req.params.ifNoneMatch of
    Just etag | etag == current -> pure (notModified current)
    _                           -> readWithEtag uri current
```

Client-side, in MCP SDKs: the JS/Python clients gain a `setSessionState` and an internal cache keyed by resource URI. Both are backwards-compat features.

## Open questions

1. **Should `_session` live in `params` (request-level) or `arguments` (per-tool field)?** Per-tool fields are easier to type; request-level reads more cleanly as session-scoped metadata. I lean request-level (matches the orthogonality of `_meta` already accepted in some MCP drafts).

2. **Etag semantics: strong vs weak.** For UCM, branch causal-hash is the natural strong etag. For other servers, a weak etag (last-updated timestamp + nonce) may be all that's available. Spec should permit both.

3. **Schema-defaulting precedence.** When a tool's args declare `projectContext` as required and `_session` provides one, is the call valid even without an explicit `arguments.projectContext`? I propose: yes — the resolved value (after merging `_session`) is what gets validated.

4. **Server-side state mutation semantics.** Should the server be allowed to set state opaquely (without the client asking)? The `notifications/session/stateChanged` mechanism above says yes; the alternative is that all state is client-driven (no server pushes). The former gives nicer ergonomics (`project-rename` can update the context automatically); the latter is conceptually simpler.

5. **Composition with `resources/templates`.** Resource URIs in MCP are template-substituted client-side. The etag should be keyed on the *substituted* URI, not the template. Worth being explicit.

## Path to upstream

1. **Discussion thread** on modelcontextprotocol/specification, linking to this document.
2. **Reference implementation PR** against this fork (~150 LOC server-side, smoke tests against `~/.local/bin/ucm`).
3. **Spec PR** with the canonical JSON Schema additions, capability strings, and backwards-compat language.
4. **Client SDK PRs** (TypeScript + Python) once the spec changes are accepted.

## Why this matters

Agent loops scale poorly when each call carries the entire session worldview and the server re-walks state that hasn't changed. Both problems show up *immediately* on any MCP server with stateful operations (any database driver, any orchestration tool, any codebase manipulation tool). The two extensions are the smallest changes to the protocol that close the cost gap without breaking existing clients.

The ucm-mcp fork has 52 tools, ~80% taking `projectContext`, and the structural-analysis ones (`diagnose`, `find`, `detect-stale`, `list-*`) walk large branch state. It is a stress test for the protocol's stateful-server story, and that's why we're surfacing this from here.
