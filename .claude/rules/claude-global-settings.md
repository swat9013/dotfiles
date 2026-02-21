---
paths: .claude-global/settings.json
---

# settings.json ガイド

## 基本

- `$schema` を常に含める: `"$schema": "https://json.schemastore.org/claude-code-settings.json"`

## 設定スコープ

| ファイル | 適用範囲 | git管理 |
|---------|---------|---------|
| `~/.claude/settings.json` | ユーザー全体 | 対象外 |
| `.claude/settings.json` | プロジェクト共有 | 対象 |
| `.claude/settings.local.json` | 個人プロジェクト | 自動gitignore |

## settings.json vs CLAUDE.md

| 判断基準 | settings.json | CLAUDE.md |
|---------|---------------|-----------|
| 読み手 | 機械（Claude Code本体） | Claude（AIモデル） |
| 記載内容 | 権限、hooks登録、UI設定 | コーディング原則、環境情報 |
| 形式 | JSON | Markdown |
| 例 | `"deny": ["Read(.env)"]` | 「rm → rmtrash で誤削除防止」 |

## 主要設定

| キー | 用途 | 例 |
|-----|------|-----|
| `permissions.allow` | 自動許可するツール呼び出し | `"Bash(npm test:*)"` |
| `permissions.deny` | 禁止するツール呼び出し | `"Read(.env)"` |
| `permissions.ask` | 確認を求めるツール呼び出し | `"Bash(git push:*)"` |
| `hooks` | イベント駆動の自動処理 | 下記参照 |
| `model` | デフォルトモデル | `"opus"`, `"sonnet"` |
| `language` | 応答言語 | `"japanese"` |
| `env` | 環境変数設定 | `{"KEY": "value"}` |
| `permissions.defaultMode` | デフォルト権限モード | `"default"`, `"allowEdits"` |
| `enabledPlugins` | プラグイン有効/無効 | `{"name@author": true}` |

## hooks設定

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "your-command",
            "timeout": 30
          }
        ]
      }
    ]
  }
}
```

主要イベント: `SessionStart`, `PreToolUse`, `PostToolUse`, `Stop`, `SubagentStart`, `SessionEnd`

hook type: `command`（シェル実行）, `prompt`（LLM評価）, `agent`（マルチターン）

## Skill権限制御

| 形式 | 許可範囲 |
|------|---------|
| `"Skill"` | 全スキル |
| `"Skill(name)"` | 引数なし呼び出しのみ |
| `"Skill(name *)"` | 任意引数での呼び出し |

## MCP設定

```json
{
  "mcpServers": {
    "server-name": {
      "command": "npx",
      "args": ["-y", "package-name"],
      "env": {}
    }
  }
}
```

| スコープ | 配置先 | 用途 |
|---------|-------|------|
| ユーザー | `~/.claude/settings.json` | 全プロジェクト共通のMCP |
| ローカル | `.claude/settings.local.json` | 個人用MCP（gitignore） |
| プロジェクト | `.claude/settings.json` | チーム共有MCP |

## UI設定

| キー | 用途 |
|-----|------|
| `statusLine` | ステータスライン表示（`type: "command"` + `command`） |
| `fileSuggestion` | @参照時のファイル候補（`type: "command"` + `command`） |

## セキュリティ

- 機密ファイルは `deny` に追加
- 広範なBash許可を避ける（`"Bash(*)"` は禁止）
- 危険な操作は `ask` に設定
