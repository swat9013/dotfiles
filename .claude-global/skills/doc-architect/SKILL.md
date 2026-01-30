---
name: doc-architect
description: |
  Diátaxisフレームワークに基づき、サービス利用者向けドキュメントの構造設計・作成を支援する。
  「ドキュメント設計」「docs設計」「Wiki構造」「Diátaxis」と依頼された時に使用。
disable-model-invocation: true
---

# doc-architect

## 概要

社内ITサービス（データ基盤、API、ツール等）の利用者向けドキュメントを、Diátaxisフレームワークに基づいて設計・作成支援するスキル。

## 前提条件

以下を確認してから実行。満たさない場合はユーザーに確認:

1. 対象サービス/プロジェクトが明確
2. ドキュメントの対象読者が特定できる
3. ドキュメントの配置先（リポジトリ、Wiki等）が決まっている

## ワークフロー

### Phase 1: 要件ヒアリング

以下を確認:

1. **サービス概要**: 何をするサービスか（1-2文）
2. **対象ユーザー**: 誰が読むか（開発者、データアナリスト、エンドユーザー等）
3. **既存ドキュメント**: 現状のドキュメント有無と課題
4. **優先タイプ**: 最初に作成すべきドキュメントタイプ

### Phase 2: 構造設計

1. Diátaxisの4タイプを説明し、対象サービスに適用:
   - **Tutorial**: 初心者向け学習コンテンツ
   - **How-to**: 特定タスク達成の手順書
   - **Reference**: 仕様・API一覧
   - **Explanation**: 背景・設計思想

2. ディレクトリ構造を提案:

```
docs/
├── index.md                 # ランディングページ
├── getting-started/         # Tutorials
│   ├── overview.md
│   ├── quickstart.md
│   └── first-project.md
├── guides/                  # How-to guides
│   ├── common-tasks/
│   └── troubleshooting.md
├── reference/               # Reference
│   ├── api/
│   ├── configuration.md
│   └── glossary.md
├── concepts/                # Explanation
│   ├── architecture.md
│   └── data-model.md
└── support/
    ├── faq.md
    └── changelog.md
```

3. ユーザーと構造を合意

### Phase 3: ドキュメント作成

1. 作成するドキュメントを選択（ユーザー指定 or 推奨順）
2. 該当タイプのテンプレートを適用（→ `references/templates.md`）
3. コンテンツを執筆
4. 関連ドキュメントへのリンクを追加

## ドキュメントタイプ判定ガイド

| ユーザーの状態 | 適切なタイプ |
|---------------|-------------|
| 「始めたい」「試したい」 | Tutorial |
| 「○○したい」「○○する方法」 | How-to |
| 「仕様は？」「パラメータは？」 | Reference |
| 「なぜ？」「仕組みは？」 | Explanation |

## 出力形式

### ディレクトリ構造提案時

```markdown
## 推奨ディレクトリ構造

[構造図]

## 優先作成ドキュメント

1. [ドキュメント名] - [理由]
2. ...
```

### ドキュメント作成時

選択したタイプのテンプレートに従ってMarkdown形式で出力。

## 成功基準

1. 4つのドキュメントタイプが適切に分類されている
2. ユーザージャーニーに沿った導線が設計されている
3. 各ドキュメントが一貫したフォーマットに従っている
4. 関連ドキュメントへのリンクが適切に設定されている

## 参考資料

- `references/diataxis-framework.md`: フレームワーク詳細
- `references/templates.md`: 各タイプのテンプレート
