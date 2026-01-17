---
paths: .claude-global/skills/**
---

# グローバル Skills ルール

## 制約

- **SKILL.md**: 500行以下
- **Progressive Disclosure**: 詳細は `references/` に分離
- **トリガー必須**: descriptionにキーワード明記

## skills vs agents

| skills | agents |
|--------|--------|
| コンテキスト共有 | コンテキスト分離 |
| 手順定義、ガイド | 並列実行、専門役割 |

→ サブエージェントはskill内でTask toolを呼び出す

## スキル一覧

| スキル | トリガー |
|--------|---------|
| architect | 「設計して」「plan.md作成」 |
| breakdown | 「/breakdown」「タスク分解」 |
| implement | 「/implement」「タスク実行」 |
| code-review | 「コードレビュー」「レビューして」 |
| researcher | 「調査して」「リサーチして」 |
| debug | 「デバッグ」「バグ調査」 |
| critical-think | 「批判的に見て」「自己レビュー」 |
| domain-context | 「学びを保存」「ドメイン知識更新」 |
| context-optimizer | 「CLAUDE.md圧縮」「コンテキスト最適化」 |
| config-optimizer | 「設定を最適化」 |
| skill-creator | 「スキルを作って」 |
| task | 「/task」「タスク追加」 |
