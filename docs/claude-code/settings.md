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

## チェック観点の優先度

| 優先度 | 意味 | 例 |
|--------|------|-----|
| Critical | 動作不能・セキュリティリスク | frontmatter欠損、機密ファイル公開 |
| High | 機能低下・トリガー不発 | description不明確、ツール権限過剰 |
| Medium | 保守性低下 | 行数超過、冗長性 |
| Low | スタイル・改善提案 | 命名、コメント |
| Info | ベストプラクティス提案 | 最適化アイデア |
