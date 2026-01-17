# コードレビュー出力テンプレート

## フォーマット

```markdown
# コードレビュー結果: [対象]

## Issues

### 1. `file.ts:42` - [問題の簡潔な説明]
**観点**: security | correctness | design | testing
**問題**: [具体的な問題点]
**根拠**: [なぜこれが問題と判断したか - コードの具体的な箇所を引用]
**修正案**: [推奨する修正方法]

### 2. ...

## 総評
指摘: X件
観点別: security X, correctness X, design X, testing X
```

## 必須フィールド

| フィールド | 説明 |
|-----------|------|
| ファイル:行番号 | 正確な位置（存在確認済み） |
| 観点 | security / correctness / design / testing |
| 問題 | 何が問題か（1-2文） |
| 根拠 | なぜ問題と判断したか（コード引用必須） |
| 修正案 | 推奨する修正方法 |

## Issueなしの場合

```markdown
# コードレビュー結果: [対象]

## Issues

指摘なし。高信号基準を満たす問題は検出されませんでした。

チェック済み観点:
- security
- correctness
- design
- testing
```
