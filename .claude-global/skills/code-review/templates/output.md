# コードレビュー出力テンプレート

## フォーマット

```markdown
# コードレビュー結果: [対象]

## Critical
### 1. `file.ts:42` - [問題の簡潔な説明] (確信度: 9/10)

**観点**: security | reliability | performance | architecture | quality
**問題**: [具体的な問題点]
**根拠**: [なぜこれが問題と判断したか - コードの具体的な箇所を引用]

**修正案**:
| 選択肢 | 概要 | 理由 |
|--------|------|------|
| A (推奨) | [修正案A] | [なぜこれが最も推奨されるか] |
| B | [修正案B] | [利点と欠点] |

## High / Medium / Low
（同様の形式）

## 総評
指摘: Critical X, High X, Medium X, Low X
観点別: security X, reliability X, performance X, architecture X, quality X
次のアクション: [推奨事項]

## 参照したドキュメント
- `~/.claude/skills/_shared/checklists/code-review/[観点].md`
- （読み込んだすべてのドキュメントを列挙）
```

## 必須フィールド

各指摘には以下を必ず含める:

| フィールド | 説明 |
|-----------|------|
| ファイル:行番号 | 正確な位置（存在確認済み） |
| 観点 | security / reliability / performance / architecture / quality |
| 問題 | 何が問題か（1-2文） |
| 確信度 | 1-10のスコア |
| 根拠 | なぜ問題と判断したか（コード引用必須） |
| 修正案テーブル | 選択肢・概要・理由（推奨は選択肢名に明記） |
