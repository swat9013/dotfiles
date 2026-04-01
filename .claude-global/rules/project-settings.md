---
paths: **/.claude/settings.json, **/.claude/settings.local.json
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
| 例 | `"deny": ["Read(.env)"]` | 「破壊的git操作は git stash で退避」 |

## 主要設定

| キー | 用途 | 例 |
|-----|------|-----|
| `permissions.allow` | 自動許可 | `"Bash(npm test *)"` |
| `permissions.deny` | 禁止 | `"Read(.env)"` |
| `permissions.ask` | 確認要求 | `"Bash(git push *)"` |
| `hooks` | イベント駆動処理 | 下記参照 |
| `model` | デフォルトモデル | `"opus"`, `"sonnet"` |
| `language` | 応答言語 | `"japanese"` |
| `env` | 環境変数 | `{"KEY": "value"}` |
| `permissions.defaultMode` | 権限モード | `"default"`, `"allowEdits"` |
| `enabledPlugins` | プラグイン制御 | `{"name@author": true}` |
| `effortLevel` | effortレベル永続化 | `"low"`, `"medium"`, `"high"` |
| `attribution` | 帰属表示設定 | `includeCoAuthoredBy` の後継 |

## hooks設定

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [{ "type": "command", "command": "your-command", "timeout": 30 }]
      }
    ]
  }
}
```

主要イベント: `PreToolUse`, `PostToolUse`, `Stop`, `SessionStart`, `SubagentStart`, `SessionEnd`

**`if` フィールド**（v2.1.85+）: パーミッションルール構文で条件フィルタリング。例: `"if": "Bash(git *)"`

| type | 用途 | 特徴 |
|------|------|------|
| `command` | 静的・形式検証 | シェル実行、最速 |
| `prompt` | 定型チェック | Haiku 1ターン、低コスト |
| `agent` | 深いレビュー | マルチターン、Read/Grep/Glob使用可 |
| `http` | 外部サービス連携 | JSON経由リクエスト/レスポンス |

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
    "server-name": { "command": "npx", "args": ["-y", "package-name"], "env": {} }
  }
}
```

| スコープ | 配置先 | 用途 |
|---------|-------|------|
| ユーザー | `~/.claude/settings.json` | 全プロジェクト共通 |
| ローカル | `.claude/settings.local.json` | 個人用（gitignore） |
| プロジェクト | `.claude/settings.json` | チーム共有 |

## settings.local.json 更新フロー

1. 現在の設定を読み込み
2. 提案する変更を生成
3. `jq .` で構文検証
4. 差分を提示
5. 承認後に `jq` でマージ更新（直接書き込み禁止）

## Bashパーミッション構文

ルールタイプは2種類のみ。グロブ（`*`、`**`）は機能しない。

| 形式 | タイプ | マッチ条件 |
|------|--------|-----------|
| `Bash(git:*)` | prefix | `git`、`git status`、`git diff HEAD` など |
| `Bash(git status)` | exact | `git status` のみ（完全一致） |

- **prefix形式**: `cmd:*` → `cmd` 単独 OR `cmd ` で始まる全コマンド
- **exact形式**: `cmd foo` → 文字列 `cmd foo` との完全一致のみ（`*` はリテラル）
- スクリプトファイル許可: `"Bash(~/.dotfiles/scripts/foo.sh:*)"` で引数あり/なし両対応

## セキュリティ

- 機密ファイルは `deny` に追加
- `"Bash(*)"` の広範許可は禁止
- 危険な操作は `ask` に設定
