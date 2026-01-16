---
paths: .claude-global/settings.json
---

# settings.json ガイド

## settings.json vs CLAUDE.md

| 判断基準 | settings.json | CLAUDE.md |
|---------|---------------|-----------|
| 読み手 | 機械（Claude Code本体） | Claude（AIモデル） |
| 記載内容 | 権限、hooks登録、UI設定 | コーディング原則、環境情報 |
| 形式 | JSON | Markdown |
| 例 | `"deny": ["Read(.env)"]` | 「rm → rmtrash で誤削除防止」 |

## 主要設定

| キー | 用途 |
|-----|------|
| `permissions.allow` | 自動許可するツール呼び出し |
| `permissions.deny` | 禁止するツール呼び出し |
| `permissions.ask` | 確認を求めるツール呼び出し |
| `hooks` | イベント駆動の自動処理 |
| `statusLine` | ステータスライン表示設定 |
| `fileSuggestion` | @参照時のファイル候補設定 |
| `enabledPlugins` | プラグインの有効/無効 |

## セキュリティ

- 機密ファイルは `deny` に追加
- 広範なBash許可を避ける
- 危険な操作は `ask` に設定
