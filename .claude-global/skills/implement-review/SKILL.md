---
name: implement-review
description: 実装→レビュー→修正の統合ワークフロー。「/implement-review」「実装してレビューまで」と依頼された時に使用。issueゼロまで自動修正サイクルを実行。
disable-model-invocation: true
---

# /implement-review コマンド

implementation.md のタスク実行後、コードレビューと修正を自動で繰り返し、品質を担保する統合ワークフロー。

## 役割: 統合オーケストレーター

**あなたはマネージャーであり、ワークフロー全体を統括するオーケストレーターです。**

### 絶対ルール

- **コードの実装・修正は絶対に自分でやらない**
- **テスト・Lintの実行は絶対に自分でやらない**
- **レビュー自体は絶対に自分でやらない**
- **上記はすべてサブエージェントまたはスキルに委譲する**

### 行動の3分類

| 分類 | 行動 | ツール例 |
|------|------|---------|
| **自身で行う** | 情報収集、判断、報告 | Read, 出力 |
| **サブエージェント委譲** | 修正、テスト実行、Lint | Task tool |
| **スキル委譲** | 実装、レビュー | Skill tool |

### やること / やらないこと

| やること | やらないこと |
|---------|-------------|
| codex-review.md を読む（Read） | コードの実装・修正 |
| issue有無の判定 | テスト・Lintの直接実行 |
| サイクル継続の判断 | レビューの直接実施 |
| スキル/サブエージェントへの委譲 | 新規ファイル作成 |
| ユーザーへの進捗報告 | - |

## 前提条件

以下を確認してから実行。満たさない場合は中止し理由を報告:

1. implementation.md が存在する
2. タスクに種別・成功基準が明記されている

## 実行手順

### Phase 1: 実装（スキル委譲）

**Skill tool で `/implement` を実行。**

自分では実装せず、implementスキルに全面委譲。

完了条件:
- 全タスクが completed ステータス
- 全品質ゲートをクリア

### Phase 2: レビュー・修正サイクル

最大 **3回** まで以下を繰り返す:

#### Step 2.1: アーキテクチャレビュー（スキル委譲）

**Skill tool で `/codex-code-review` を実行。**

自分ではレビューせず、codex-code-reviewスキルに全面委譲。

codex-review.md が生成される（4観点：Architecture、Test Strategy、API Design、Behavior）。

#### Step 2.2: issue判定（自身で実行）

Read tool で codex-review.md を読み込み、issue の有無を判定:

- **issueなし**: Phase 3 へ進む
- **issueあり**: issue一覧を抽出し、Step 2.3 へ

#### Step 2.3: 自動修正（サブエージェント委譲）

**自分では修正しない。すべてサブエージェントに委譲する。**

codex-review.md の各 issue に対して修正サブエージェントを起動:

```
# 異なるファイルへの修正は単一メッセージで並列起動
Task tool × N（issue数）

subagent_type: general-purpose
model: sonnet
prompt: |
  あなたはコード修正の専門家です。

  ## 修正対象
  ファイル: ${file}
  行番号: ${line}

  ## issue内容
  観点: ${dimension}  # architecture, test_strategy, api_design, behavior
  問題: ${problem}
  根拠: ${evidence}
  修正案: ${suggestion}

  ## 指示
  1. 該当箇所を特定
  2. 修正案に基づいて修正を実施
  3. 周辺コードとの整合性を確認
  4. 変更内容を報告

  修正後、既存のテストを破壊しないこと。
```

#### Step 2.4: 品質ゲート（サブエージェント委譲）

**自分では実行しない。サブエージェントに委譲する。**

単一メッセージで2つのTask toolを並列起動:

```
# Lint チェック
subagent_type: general-purpose
model: haiku
prompt: |
  Lintを実行: ${LINT_COMMAND}
  結果を報告（成功/失敗、エラー詳細）

# Test チェック
subagent_type: general-purpose
model: sonnet
prompt: |
  テストを実行: ${TEST_COMMAND}
  結果を報告（成功/失敗、失敗テスト詳細）
```

**失敗時**: 修正サブエージェントを起動して修正を委譲（自分では修正しない）

#### Step 2.5: サイクル継続判定（オーケストレーターが判断）

- 品質ゲートクリア → Step 2.1 へ戻る
- 最大回数到達 → Phase 3 へ進む（警告を表示）

### Phase 3: 完了

#### 成功時（issueゼロ）

```markdown
## 実装・レビュー完了

**実装**: 全タスク完了
**レビュー**: issueゼロ
**修正サイクル**: X回

**品質ゲート**: 全クリア

**次のステップ**:
- PR作成
```

#### 最大回数到達時

```markdown
## 実装・レビュー完了（警告あり）

**実装**: 全タスク完了
**レビュー**: X件のissueが残存
**修正サイクル**: 3回（上限到達）

**残存issue**:
- `file.ts:42` - [問題の簡潔な説明]

**推奨アクション**:
- 手動で残存issueを確認
- 必要に応じて `/codex-code-review` を再実行
```

## 成功基準

1. implementation.md の全タスクが completed
2. codex-review.md の issue がゼロ（または最大サイクル後に残存を報告）
3. 全品質ゲートをクリア

## 制約

- 最大修正サイクル: 3回
- 同一 issue が2回以上検出された場合、手動対応を推奨（警告表示）
