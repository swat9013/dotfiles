---
name: implement
description: implementation.mdのタスクを並列実行。品質ゲート付きでPhase別に実行。「/implement」「タスク実行」と依頼された時に使用。
---

# /implement コマンド

implementation.md のタスクを Phase 別に実行する。並列実行可能タスク（Pマーク）は同時実行し、品質ゲートで各フェーズの品質を担保する。

## 前提条件

以下を確認してから実行。満たさない場合は中止し理由を報告:

1. implementation.md が存在する
2. タスクに種別・成功基準が明記されている

## 実行手順

### 1. 前提条件チェック（Haiku）

Task tool で軽量エージェントを起動:
- model: haiku
- prompt: |
    implementation.md の存在と形式を検証:
    1. ファイルが存在するか
    2. タスクに種別・成功基準が明記されているか
    3. 各タスクのステータス（pending/in_progress/completed）を集計
    結果を報告（OK or 問題点リスト）

### 2. implementation.md 読み込み
- タスク一覧、依存関係、並列実行可否を把握
- 現在のステータス（pending/in_progress/completed）を確認

### 3. Phase別実行サイクル

```
Phase開始 → 並列タスク実行 → 順次タスク実行 → 品質ゲート → 進捗報告 → 次Phase
```

### 4. タスク実行

実行中に不明点があれば `[NEEDS CLARIFICATION: 質問]` を挿入し、ユーザーに確認（最大3箇所）。

#### 並列実行（Pマーク付きタスク）
Task tool で複数サブエージェントを**単一メッセージで同時呼び出し**:
- タスク内容、成功基準、制約条件を渡す
- **タスク種別に応じたロール・原則・モデルを付与**（`guides/task-roles.md` 参照）

#### 順次実行（依存タスク）
依存タスク完了後に実行。

### 5. 品質ゲート（並列実行）

各Phase完了時に以下を**単一メッセージで並列起動**:

| チェック | モデル | コマンド |
|---------|--------|---------|
| Lint | Haiku | implementation.md記載の LINT_COMMAND |
| Test | Sonnet | implementation.md記載の TEST_COMMAND |
| Build | Haiku | implementation.md記載の BUILD_COMMAND（該当時） |

**注**: コマンドはimplementation.mdまたはプロジェクトのCLAUDE.mdから取得。

失敗時: 原因分析 → 修正 → 再検証

### 6. ステータス更新

implementation.md のタスクステータスを更新:
- 実行中: `in_progress`
- 完了: `completed`

### 7. Phase完了報告

```markdown
## Phase X 完了報告

### 実施内容
- ✅ TASK-001: [タイトル]（完了、所要時間: 2h）
- ✅ TASK-002: [タイトル]（完了、所要時間: 1h）

### 品質ゲート結果
- ✅ Lint: 成功
- ✅ テスト: 全XX件通過

### 次Phase概要
- 並列実行可能タスク: X件
```

## タスク種別とサブエージェントのロール

タスク種別に応じて、サブエージェントにロール・原則を付与する。

→ 詳細: `guides/task-roles.md`

| 種別 | ロールの要点 |
|-----|-------------|
| 新機能実装 | TDD前提、YAGNI、最小実装 |
| リファクタリング | 既存テスト維持、段階的変更 |
| テスト追加 | Observable Behavior、境界値 |
| バグ修正 | 原因特定優先、回帰テスト |
| ドキュメント | 整合性、簡潔さ |

## 成功基準

このコマンドの実行は以下を満たしたとき成功とみなす:

1. 全タスクが completed ステータスになっている
2. 全 Phase の品質ゲートをクリアしている
3. plan.md の成功基準を満たしている

## 完了チェックリスト

- [ ] 全Phaseのタスクが completed
- [ ] 全品質ゲートをクリア
- [ ] implementation.md のステータスが更新済み
- [ ] plan.md の成功基準を満たしている

## 出力形式

```
## 実装完了

**完了タスク**: X個
**総所要時間**: Xh
**品質ゲート**: 全クリア

**次のステップ**:
- /code-review でコードレビュー
- PR作成
```
