# settings.json 設定ガイド

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

## セキュリティ

- 機密ファイルは`deny`に追加
- 広範なBash許可を避ける
- 危険な操作は`ask`に設定

<!-- TODO: permissions詳細（allow/deny/ask パターン、ツール別推奨設定） -->
<!-- TODO: MCP設定（mcpServers設定パターン） -->
<!-- TODO: 環境変数（CLAUDE_CODE_* 系の設定可能な変数一覧） -->
<!-- TODO: プロジェクト別設定 vs グローバル設定の使い分け -->
