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

引数ワイルドカードは**スペース + `*`** を使う。`:*` は非推奨（リテラルのコロンとして扱われマッチしない）。

| パターン | マッチする | マッチしない |
|---------|-----------|-------------|
| `Bash(git diff *)` | `git diff HEAD` | `git diffstat` |
| `Bash(git diff)` | `git diff`（完全一致のみ） | `git diff HEAD` |
| `Bash(./scripts/dir/* *)` | `./scripts/dir/foo.sh arg1` | `./scripts/dir/`（引数なし） |
| `Bash(npm run *)` | `npm run build`, `npm run test` | `npm install` |

注意点:
- `Bash(cmd *)` — スペース区切りで `cmd foo` にマッチ
- `Bash(cmd*)` — スペースなしで `cmdfoo` にもマッチ（語境界注意）
- ディレクトリ内スクリプト全許可: `"Bash(./scripts/dir/* *)"` （`/*` でファイル名、` *` で引数）

## セキュリティ

- 機密ファイルは`deny`に追加
- 広範なBash許可を避ける（`"Bash(*)"` は禁止）
- 危険な操作は`ask`に設定

<!-- TODO: MCP設定（mcpServers設定パターン） -->
<!-- TODO: 環境変数（CLAUDE_CODE_* 系の設定可能な変数一覧） -->
