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
| 例 | `"deny": ["Read(.env)"]` | 「破壊的git操作は git stash で退避」 |

## 主要設定

| キー | 用途 | 例 |
|-----|------|-----|
| `permissions.allow` | 自動許可するツール呼び出し | `"Bash(npm test *)"` |
| `permissions.deny` | 禁止するツール呼び出し | `"Read(.env)"` |
| `permissions.ask` | 確認を求めるツール呼び出し | `"Bash(git push *)"` |
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

hook type と使い分け:

| type | 用途 | 特徴 |
|------|------|------|
| `command` | 静的・形式検証 | シェル実行、最速 |
| `prompt` | 定型チェック | Haiku 1ターン、低コスト |
| `agent` | 深いレビュー（ファイル読み込みあり） | マルチターン、最大50ターン |

応用例: `PreToolUse` の matcher に `ExitPlanMode` を指定 → plan提案前にレビュー自動挿入

## settings.local.json の安全な更新フロー

1. 現在の設定を読み込み
2. 提案する変更を生成
3. `jq .` で JSON 構文検証
4. 現在値 → 提案値の差分を提示
5. 承認後に `jq` でマージ更新（直接書き込み禁止）

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

## Bashパーミッション構文

ルールタイプは2種類のみ。グロブ（`*`、`**`）は機能しない。

| 形式 | タイプ | マッチ条件 |
|------|--------|-----------|
| `Bash(git:*)` | prefix | `git`、`git status`、`git diff HEAD` など |
| `Bash(git status)` | exact | `git status` のみ（完全一致） |

- **prefix形式**: `cmd:*` → `cmd` 単独 OR `cmd ` で始まる全コマンド
- **exact形式**: `cmd foo` → 文字列 `cmd foo` との完全一致のみ（`*` はリテラル）
- スクリプトファイル許可: `"Bash(~/.dotfiles/scripts/foo.sh:*)"` で引数あり/なし両対応

## セキュリティ（二層防御）

危険な操作は PreToolUse フック（第1層）と permissions.deny（第2層）の二層で防御する:

| 層 | 仕組み | 役割 |
|----|--------|------|
| 第1層 | `pretooluse-guard.sh` | 動的条件分岐（ブランチ判定等）、メイン防御 |
| 第2層 | `permissions.deny` | 静的マッチング、フック障害時のフォールバック |

- フックでカバーする操作は `deny` にも登録する（フック未実行時の安全網）
- 機密ファイルは Read/Write/Edit すべて `deny` に追加
- 広範なBash許可を避ける（`"Bash(*)"` は禁止）
- `rm` はシェルalias（`rm='rmtrash'`）で透過的にゴミ箱へ移動される
