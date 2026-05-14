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
- `C-c a h` - Show all usage stats (24h/7d/30d) in a dedicated buffer
- `C-c a 1` - Show last 24 hours usage
- `C-c a 7` - Show last 7 days usage
- `C-c a 3` - Show last 30 days usage (mnemonic: ~3=30 days)

## Interactive Commands

- `M-x agent-shell-usage-history-show-all` - View all time periods in one buffer
- `M-x agent-shell-usage-history-show-daily` - Last 24 hours
- `M-x agent-shell-usage-history-show-weekly` - Last 7 days  
- `M-x agent-shell-usage-history-show-monthly` - Last 30 days
- `M-x agent-shell-usage-history-export` - Export to CSV
- `M-x agent-shell-usage-history-clear-old` - Remove records older than N days

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
```

## How It Works

1. After each agent turn, `agent-shell--save-usage` is called by agent-shell
2. Our advice wrapper automatically persists this to disk
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
