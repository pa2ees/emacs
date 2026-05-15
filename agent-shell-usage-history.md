# Agent Shell Usage History

Persistent usage tracking across agent-shell sessions with aggregate views and session-level detail.

## Overview

`agent-shell-usage-history` automatically records token usage, costs, and context for every agent-shell turn. Data is stored in weekly JSONL files and can be viewed by day, week, month, or per-session.

## Key Bindings

All commands are under `C-c a U`:

- `C-c a U a` - Show all usage views (current session, today, and period summaries)
- `C-c a U 1` - Show last 24 hours summary
- `C-c a U 7` - Show last 7 days summary  
- `C-c a U 3` - Show last 30 days summary
- `C-c a U e` - Export usage history to CSV
- `C-c a U c` - Clear old records (prompts for number of days to keep)
- `C-c a U d` - Toggle debug mode

## Display Views

### Current Session
Shows real-time usage for the active agent-shell session:
```
=== Current Session ===
Session: 91ed
Project: ~/projects/emacs/
Total tokens: 792K (cached: 672K)
  in: 7, out: 3.4K
Context: 110K / 200K (55.0%)
Cost: $7.11
```

### Today's Sessions
Table view of all sessions from today, grouped and sorted by project:
```
=== Today (2026-05-15) ===
Session         Tokens       Cost  Turns  Project
----------------------------------------------------------------------
91ed              792K      $7.11      1  ~/projects/emacs/
```

### Period Summaries
Aggregated statistics with unique session counting:
```
=== Last 24 Hours ===
Sessions: 1 | Total: 792K | Cost: $7.11

=== Last 7 Days ===
Sessions: 5 | Total: 2.1M | Cost: $15.43

=== Last 30 Days ===
Sessions: 23 | Total: 8.7M | Cost: $52.18
```

## Data Storage

### Location
Usage data is stored in weekly JSONL files:
```
~/.local/share/agent-shell-usage-history/
  usage-2026-W20.jsonl
  usage-2026-W19.jsonl
  ...
```

### Format
Each line is a JSON object representing one turn:
```json
{
  "timestamp": "2026-05-15T10:06:01",
  "session_id": "20260515-090515-251995-91ed",
  "project_root": "/home/erik/projects/emacs/",
  "total_tokens": 792398,
  "input_tokens": 7,
  "output_tokens": 3403,
  "thought_tokens": 0,
  "cached_read_tokens": 672447,
  "cached_write_tokens": 116541,
  "context_used": 113834,
  "context_size": 200000,
  "cost_amount": 0.69,
  "cost_currency": "USD"
}
```

### Session Tracking
- **Session ID**: Unique identifier per agent-shell session (format: `YYYYMMDD-HHMMSS-PID-RAND`)
- **Project Root**: Captured from `projectile-project-root` or `default-directory` at session start
- **Per-turn Deltas**: Token counts are per-turn; costs are calculated as deltas from cumulative usage

## Features

### Weekly Files
- One JSONL file per ISO week (e.g., `usage-2026-W20.jsonl`)
- Efficient for querying recent usage (don't need to read old data)
- Easy to archive or delete old weeks

### Efficient Appending
- JSONL format allows O(1) append operations
- No need to read/parse entire file to add a record
- Concurrent-safe with retry logic and file locking

### Session Grouping
- Multiple turns in same session share the same session ID
- Display views correctly count unique sessions, not individual turns
- Today view shows session-level aggregates

### Cost Delta Calculation
- Agent-shell provides cumulative costs; we calculate per-turn deltas
- First turn in session shows full cost, subsequent turns show incremental cost
- Makes it easy to see cost per interaction

### Debug Mode
- Toggle with `C-c a U d`
- Logs to `*Agent Shell Usage History Debug*` buffer
- Shows record saves, cost calculations, and file operations

## Export

Export all usage data to CSV:
```
C-c a U e
```

Prompts for filename (defaults to `agent-shell-usage-YYYYMMDD.csv`). Exports all fields including session ID and project root.

## Maintenance

### Clear Old Records
Remove records older than N days:
```
C-c a U c
```

Deletes entire weekly files that contain no records newer than the cutoff date.

### File Cleanup
Weekly files can be manually deleted from:
```
~/.local/share/agent-shell-usage-history/
```

## Implementation Notes

### Concurrent Safety
- Uses `append-to-file` with OS-level O_APPEND for atomic writes
- Retry logic (5 attempts with 0.1s delays) handles transient lock conflicts
- Safe for multiple Emacs instances writing to same weekly file

### Event Subscription
- Subscribes to `turn-complete` events from agent-shell
- Automatically tracks usage without manual intervention
- Session ID and project root initialized on `agent-shell-mode-hook`

### Per-turn Data
- Agent-shell provides cumulative usage; we store per-turn deltas for costs
- Uses `copy-tree` to snapshot usage data (prevents mutation issues)
- Token counts are already per-turn from agent-shell

## Customization

```elisp
;; Change storage location
(setq agent-shell-usage-history-directory
      (expand-file-name "my-custom-path/usage-history"))

;; Enable debug mode by default
(setq agent-shell-usage-history-debug t)
```
