---
paths: .claude-global/skills/**
---

# Skills ガイド

## skills vs agents vs rules

| 観点 | skills/ | agents/ | rules/ |
|------|---------|---------|--------|
| 用途 | ワークフロー・手順定義 | カスタムサブエージェント | パス固有のルール |
| 実行形態 | 対話型・相談型 | コンテキスト分離実行 | 自動適用 |
| トリガー | Claude自動選択 or `/skill-name` | Task tool経由 | ディレクトリアクセス時 |

## skills vs agents

| 判断基準 | skills | agents |
|---------|--------|--------|
| コンテキスト | 共有（会話継続） | 分離（独立実行） |
| 適用場面 | 手順定義、ガイド | 並列実行、試行錯誤、専門役割 |

## _shared/ リソース配置基準

- **2つ以上のスキルで使う**: `_shared/` に配置
- **1つのスキルのみ**: skill固有ディレクトリに配置

## SKILL.md 制約

- **上限**: 500行
- **Progressive Disclosure**: 詳細は `references/` に分離
- **トリガー明記**: `description` にキーワード含める

## 参照先の明示

詳細を references/ に分離した場合、参照先パスを明記:
- 「詳細は `references/xxx.md` を参照」
- リンク切れを防ぐため相対パスを使用

## ディレクトリ構成

```
skill-name/
├── SKILL.md           # 必須: 手順・ガイドライン
├── references/        # 参照ドキュメント（必要時のみ読み込み）
├── scripts/           # 実行スクリプト（読み込まず実行）
└── assets/            # テンプレート・画像等
```

## スキル一覧

| スキル名 | 用途 | トリガーキーワード |
|---------|------|------------------|
| code-review | 5観点コードレビュー | 「コードレビュー」「レビューして」 |
| config-optimizer | 設定最適化と改善提案 | 「設定を最適化」「config-optimizer」 |
| domain-context | プロジェクトドメイン知識管理 | 「学びを保存」「ドメイン知識更新」 |
| context-optimizer | CLAUDE.mdの圧縮・最適化 | 「CLAUDE.md圧縮」「コンテキスト最適化」またはdomain-contextから自動呼び出し |
| critical-think | 回答の批判的検証 | 「批判的に見て」「自己レビュー」 |
| debug | 根本原因分析（5 Whys） | 「デバッグ」「バグ調査」 |
| architect | 設計・plan.md作成 | 「設計して」「plan.md作成」 |
| breakdown | 実装タスク分解 | 「/breakdown」「タスク分解」 |
| implement | implementation.md実行 | 「/implement」「タスク実行」 |
| researcher | 技術調査・分析 | 「調査して」「リサーチして」 |
| skill-creator | スキル開発支援 | 「スキルを作って」 |
| task | プロジェクトタスク管理 | 「/task」「タスク追加」 |
