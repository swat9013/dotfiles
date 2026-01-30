---
name: implement-review
description: 実装→レビュー→修正の統合ワークフロー。「/implement-review」「実装してレビューまで」と依頼された時に使用。issueゼロまで自動修正サイクルを実行。
disable-model-invocation: true
---

# /implement-review コマンド

implementation.md のタスク実行後、コードレビューと修正を自動で繰り返し、品質を担保する統合ワークフロー。

## 前提条件

以下を確認してから実行。満たさない場合は中止し理由を報告:

1. implementation.md が存在する
2. タスクに種別・成功基準が明記されている

## 実行手順

### Phase 1: 実装

Skill tool で `/implement` を実行。

完了条件:
- 全タスクが completed ステータス
- 全品質ゲートをクリア

### Phase 2: レビュー・修正サイクル

最大 **3回** まで以下を繰り返す:

#### Step 2.1: アーキテクチャレビュー

Skill tool で `/codex-code-review` を実行。

codex-review.md が生成される（4観点：Architecture、Test Strategy、API Design、Behavior）。

#### Step 2.2: issue判定

codex-review.md を読み込み、issue の有無を確認:

- **issueなし**: Phase 3 へ進む
- **issueあり**: Step 2.3 へ

#### Step 2.3: 自動修正

codex-review.md の各 issue に対して修正を実行。

**観点に応じたモデル選択**:
| 観点 | モデル | 理由 |
|------|--------|------|
| architecture | Sonnet | アーキテクチャ修正は高精度必須 |
| api_design | Sonnet | API設計修正は高精度必須 |
| test_strategy | Sonnet | テスト戦略改善は標準精度で十分 |
| behavior | Sonnet | 振る舞い調整は標準精度で十分 |

**修正プロンプト構造**:
```
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

並列実行可能な issue（異なるファイルへの修正）は **単一メッセージで同時呼び出し**。

#### Step 2.4: 品質ゲート

修正後、以下を並列実行:
- Lint（implementation.md 記載の LINT_COMMAND）
- Test（implementation.md 記載の TEST_COMMAND）

失敗時: 原因分析 → 修正 → 再検証

#### Step 2.5: サイクル継続判定

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
