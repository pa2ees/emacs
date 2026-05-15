# Agent Shell Usage Tracking

This extension adds persistent usage tracking across agent-shell sessions, allowing you to view aggregated statistics by day, week, and month.

## Features

- **Automatic tracking**: Usage data is automatically saved after each agent turn
- **Multiple time periods**: View stats for last 24 hours, 7 days, or 30 days
- **Detailed metrics**: Track tokens (input, output, thought, cached), context usage, and costs
- **CSV export**: Export all usage data for external analysis
- **History management**: Clean up old records to keep the data file manageable

## Key Bindings

In any buffer with agent-shell loaded:

- `C-c a u` - Show current session usage
- `C-c a U` prefix for usage history commands:
  - `C-c a U a` - Show all usage stats (24h/7d/30d) in a dedicated buffer
  - `C-c a U 1` - Show last 24 hours usage
  - `C-c a U 7` - Show last 7 days usage
  - `C-c a U 3` - Show last 30 days usage (mnemonic: ~3=30 days)
  - `C-c a U e` - Export to CSV
  - `C-c a U c` - Clear old records
  - `C-c a U d` - Toggle debug mode

## Interactive Commands

- `M-x agent-shell-usage-history-show-all` - View all time periods in one buffer
- `M-x agent-shell-usage-history-show-daily` - Last 24 hours
- `M-x agent-shell-usage-history-show-weekly` - Last 7 days  
- `M-x agent-shell-usage-history-show-monthly` - Last 30 days
- `M-x agent-shell-usage-history-export` - Export to CSV
- `M-x agent-shell-usage-history-clear-old` - Remove records older than N days
- `M-x agent-shell-usage-history-toggle-debug` - Toggle debug output

## Data Storage

Usage history is stored in:
```
~/.local/share/agent-shell-usage-history.json
```

Or `$XDG_DATA_HOME/agent-shell-usage-history.json` if XDG_DATA_HOME is set.

## Customization

```elisp
;; Change the storage location
(setq agent-shell-usage-history-file "~/my-custom-path/usage.json")

;; Enable debug mode
(setq agent-shell-usage-history-debug t)
```

## Debugging

When debug mode is enabled (via `C-c a U d` or setting `agent-shell-usage-history-debug` to `t`), detailed debug messages are written to the `*Agent Shell Usage History Debug*` buffer. This includes:

- When events are received
- What usage data is being recorded
- File load/save operations
- Record counts and timestamps

## How It Works

1. The code subscribes to agent-shell's `turn-complete` event via `agent-shell-mode-hook`
2. After each agent turn completes, the event handler automatically persists usage to disk
3. Each record includes timestamp, token counts, context info, and cost
4. Query functions filter records by time period and aggregate the totals

## Example Output

```
=== Last 7 Days Usage ===
Sessions: 12
Total tokens: 145k
  Input: 89k
  Output: 43k
  Thought: 8k
  Cached read: 5k
Cost: $2.47
```

## Concurrent Sessions

Since each agent-shell session tracks its own usage and records are timestamped, concurrent sessions are fully supported. Each session's usage is recorded independently when turns complete.
