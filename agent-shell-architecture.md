# Agent Shell Architecture Diagram

## Complete System Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           EMACS PROCESS                                  │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────┐           │
│  │  USER INTERACTION LAYER                                  │           │
│  │                                                           │           │
│  │  ┌────────────────────────────────────────────┐          │           │
│  │  │ *agent-shell* buffer                       │          │           │
│  │  │ (agent-shell-mode)                         │          │           │
│  │  │ - User types prompts                       │          │           │
│  │  │ - Displays Claude responses                │          │           │
│  │  │ - Shows usage/cost stats                   │          │           │
│  │  └─────────────────┬──────────────────────────┘          │           │
│  │                    │                                      │           │
│  │                    │ (1) Send prompt                      │           │
│  │                    ↓                                      │           │
│  │  ┌─────────────────────────────────────────────┐         │           │
│  │  │ agent-shell core                            │         │           │
│  │  │ - Manages conversation state                │         │           │
│  │  │ - Tracks usage/tokens                       │         │           │
│  │  │ - Spawns claude-acp-engine.sh subprocess   │         │           │
│  │  └─────────────────┬─────────────────────────┘          │           │
│  │                    │                                      │           │
│  └────────────────────┼──────────────────────────────────────┘           │
│                       │                                                  │
│  ┌────────────────────┼──────────────────────────────────────┐           │
│  │  MCP SERVER LAYER  │                                      │           │
│  │                    │                                      │           │
│  │  ┌─────────────────↓─────────────────────────┐           │           │
│  │  │ emacs-mcp-server                          │           │           │
│  │  │ (mcp-server package)                      │←──────────┼─────┐     │
│  │  │                                           │           │     │     │
│  │  │ Unix Socket:                              │           │     │     │
│  │  │ /tmp/emacs-mcp-<pid>.sock                │           │     │     │
│  │  │                                           │           │  (5)│     │
│  │  │ Available Tools:                          │           │  Execute  │
│  │  │ • eval-elisp                              │           │  elisp    │
│  │  │ • org-capture, org-search                 │           │     │     │
│  │  │ • org-agenda, org-clock                   │           │     │     │
│  │  │ • get-diagnostics                         │           │     │     │
│  │  └─────────────────┬─────────────────────────┘           │     │     │
│  │                    ↑                                      │     │     │
│  └────────────────────┼───────────────────────────────────────────┼─────┘
│                       │                                            │      
│  ┌────────────────────┼───────────────────────────────────────────┼─────┐
│  │  BRIDGE LAYER      │                                           │     │
│  │                    │                                           │     │
│  │  ┌─────────────────↓─────────────────────────┐                │     │
│  │  │ MCP HTTP Bridge                           │                │     │
│  │  │ (evz/agent-shell-bridge-*)                │                │     │
│  │  │                                           │                │     │
│  │  │ HTTP Server: 127.0.0.1:8126              │                │     │
│  │  │                                           │                │     │
│  │  │ Function: Bidirectional forwarding       │                │     │
│  │  │ • HTTP POST → Unix Socket                │                │     │
│  │  │ • Unix Socket response → HTTP            │                │     │
│  │  │                                           │                │     │
│  │  │ Why? claude-acp-engine expects HTTP      │                │     │
│  │  │ but emacs-mcp-server uses Unix sockets   │                │     │
│  │  └─────────────────┬─────────────────────────┘                │     │
│  │                    ↑                                          │     │
│  └────────────────────┼────────────────────────────────────────────────┘
│                       │                                                 │
└───────────────────────┼─────────────────────────────────────────────────┘
                        │
                        │ (2) MCP-over-HTTP
                        │ (4) Tool call: HTTP POST with JSON-RPC
                        │ (6) Tool response: HTTP 200 with result
                        │
┌───────────────────────┼─────────────────────────────────────────────────┐
│ CLAUDE ACP ENGINE     │                                                 │
│                       │                                                 │
│  ┌────────────────────↓──────────────────────────┐                      │
│  │ ~/.config/claude/claude-acp-engine.sh         │                      │
│  │                                                │                      │
│  │ • Manages Claude conversation                 │                      │
│  │ • Configured with MCP server URL:             │                      │
│  │   http://127.0.0.1:8126                       │                      │
│  │ • Streams responses back to agent-shell       │                      │
│  │ • Handles tool calls from Claude              │                      │
│  └────────────────────┬──────────────────────────┘                      │
│                       │                                                 │
│                       │ (3) Bedrock API Request                         │
│                       │     + MCP server config                         │
│                       ↓                                                 │
│  ┌───────────────────────────────────────────────┐                      │
│  │ AWS SSO Authentication                        │                      │
│  │ (claude-login.sh)                             │                      │
│  └────────────────────┬──────────────────────────┘                      │
└─────────────────────────┼────────────────────────────────────────────────┘
                          │
                          │ HTTPS
                          │
                ┌─────────↓──────────┐
                │                    │
                │   AWS Bedrock      │◄─────┐
                │   Claude Model     │      │
                │                    │      │
                └─────────┬──────────┘      │
                          │                 │
                          │ (7) Response    │ (4a) "I need to call
                          │     with tool   │      tool: eval-elisp"
                          │     results     │
                          └─────────────────┘
```

## Data Flow Sequence

### Outbound (User → Claude):

1. **User types prompt** in `*agent-shell*` buffer
2. **agent-shell** spawns `claude-acp-engine.sh` subprocess with:
   - User prompt
   - MCP server config: `http://127.0.0.1:8126`
3. **ACP Engine** authenticates with AWS SSO and sends to Bedrock
4. **Claude** receives prompt and MCP server configuration

### Inbound (Claude → Emacs tools):

4a. **Claude decides** to call an Emacs tool (e.g., `eval-elisp`)
5. **Claude → ACP Engine** → HTTP POST to `http://127.0.0.1:8126`
    ```json
    {"method": "tools/call", "params": {"name": "eval-elisp", "arguments": {...}}}
    ```
6. **Bridge** forwards raw JSON to Unix socket `/tmp/emacs-mcp-<pid>.sock`
7. **MCP Server** receives request, executes elisp in Emacs
8. **Response flows back**: MCP Server → Bridge → ACP Engine → Claude
9. **Claude** continues reasoning with tool results
10. **Final response** streams back to agent-shell buffer

## Process Lifecycle

### Startup:
```
User triggers: evz/agent-shell-switch or evz/agent-shell-with-context
    ↓
evz/emacs-mcp-server-start
    → Starts Unix socket server at /tmp/emacs-mcp-<pid>.sock
    ↓
evz/agent-shell-bridge-start  
    → Starts HTTP server on port 8126
    → Configures agent-shell-mcp-servers with bridge URL
    ↓
agent-shell spawns claude-acp-engine.sh
    → ACP engine connects to Bedrock
    → Registers MCP server at http://127.0.0.1:8126
```

### Shutdown:
```
User kills *agent-shell* buffer
    ↓
evz/agent-shell-cleanup (kill-buffer-hook)
    ↓
evz/agent-shell-bridge-stop
    → Closes HTTP server
    ↓
evz/emacs-mcp-server-stop
    → Closes Unix socket
    → Cleans up /tmp/emacs-mcp-<pid>.sock
```

## Key Components

| Component | Type | Location | Purpose |
|-----------|------|----------|---------|
| agent-shell | Emacs package | ELPA | UI for Claude conversations |
| emacs-mcp-server | Emacs package | VC (GitHub) | Exposes Emacs via MCP protocol |
| MCP Bridge | Custom elisp | init_agent_shell.el | HTTP↔Unix socket adapter |
| claude-acp-engine.sh | Shell script | ~/.config/claude/ | Claude/Bedrock API client |
| claude-login.sh | Shell script | ~/.config/claude/ | AWS SSO authentication |

## Why the Bridge?

**Problem**: Impedance mismatch
- `agent-shell`'s Claude client (ACP engine) expects **HTTP** MCP servers
- `emacs-mcp-server` uses **Unix sockets** (standard for local MCP servers)

**Solution**: Lightweight HTTP→Unix socket bridge
- Accepts HTTP on 127.0.0.1:8126
- Forwards raw JSON-RPC to Unix socket
- Returns responses back over HTTP
- Zero parsing/transformation (raw passthrough)

## Configuration Files

```
~/.config/claude/
├── claude-acp-engine.sh    # Claude API client
└── claude-login.sh         # AWS SSO login

~/projects/emacs/
└── init_agent_shell.el     # All the glue code
```
