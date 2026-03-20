# settings.json 設定ガイド

## 設定スコープ

| ファイル | 適用範囲 | git管理 |
|---------|---------|---------|
| `~/.claude/settings.json` | ユーザー全体 | 対象外 |
| `.claude/settings.json` | プロジェクト共有 | 対象 |
| `.claude/settings.local.json` | 個人プロジェクト | 自動gitignore |

## 推奨設定

```json
{
  "$schema": "https://json.schemastore.org/claude-code-settings.json",
  "permissions": {
    "deny": [
      "Read(./.env)",
      "Read(**/*.key)"
    ]
  }
}
```

## Bashパーミッション構文

ルールタイプは**2種類のみ**。グロブ（`*`、`**`）は機能しない。

| パターン | タイプ | マッチする | マッチしない |
|---------|--------|-----------|-------------|
| `Bash(git:*)` | prefix | `git`, `git status`, `git diff HEAD` | （なし） |
| `Bash(git diff:*)` | prefix | `git diff`, `git diff HEAD~1` | `git status` |
| `Bash(git status)` | exact | `git status` のみ | `git status -s` |
| `Bash(git diff *)` | exact | 文字通り `git diff *` のみ | `git diff HEAD` |

- **prefix形式 `cmd:*`**: `cmd` 単独 OR `cmd ` で始まる全コマンドにマッチ
- **exact形式**: `*` を含んでいてもリテラル文字列との完全一致のみ（ワイルドカードではない）
- スクリプトファイル許可: `"Bash(~/.dotfiles/scripts/foo.sh:*)"` で引数あり/なし両対応

## セキュリティ

- 機密ファイルは`deny`に追加
- 広範なBash許可を避ける（`"Bash(*)"` は禁止）
- 危険な操作は`ask`に設定

<!-- TODO: MCP設定（mcpServers設定パターン） -->
<!-- TODO: 環境変数（CLAUDE_CODE_* 系の設定可能な変数一覧） -->
