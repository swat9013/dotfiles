# ADR (Architecture Decision Record) テンプレート

アーキテクチャ上の決定事項を記録するためのテンプレート。

## 使用タイミング

- アーキテクチャに影響する決定をしたとき
- 複数の選択肢から1つを選んだとき
- 将来「なぜこうなっているか」を説明する必要があるとき

## テンプレート

```markdown
# ADR-[番号]: [タイトル]

## Status

[Proposed | Accepted | Deprecated | Superseded by ADR-XXX]

## Context

### 背景
[どのような状況でこの決定が必要になったか]

### 制約条件
- [技術的制約]
- [ビジネス制約]
- [期限等]

### 検討した選択肢

**選択肢A: [名前]**
- 概要: [説明]
- 利点: [メリット]
- 欠点: [デメリット]

**選択肢B: [名前]**
- 概要: [説明]
- 利点: [メリット]
- 欠点: [デメリット]

## Decision

### 採用する設計
[選択した設計案]

### 理由
1. [理由1]
2. [理由2]
3. [理由3]

## Consequences

### Positive
- [良い影響1]
- [良い影響2]

### Negative
- [悪い影響1]
- [悪い影響2]

### Mitigation（負の影響の緩和策）
- [緩和策1]
- [緩和策2]

## Notes

### 関連ドキュメント
- [関連するADRやドキュメントへのリンク]

### 決定日
[YYYY-MM-DD]
```

## 保存先

`docs/decisions/ADR-[番号]-[slug].md`

例: `docs/decisions/ADR-001-use-typescript.md`

## 命名規則

- 番号: 連番（001, 002, ...）
- slug: ケバブケースの短い説明

## ステータスの意味

| Status | 意味 |
|--------|------|
| Proposed | 提案中（レビュー待ち） |
| Accepted | 承認済み（実装可能） |
| Deprecated | 非推奨（新規では使わない） |
| Superseded | 置き換え済み（新しいADRを参照） |
