---
name: review-fix
description: |-
  実装後のコードをOpusサブエージェント（コンテキスト分離）でレビューし、修正サブエージェントで自動修正するサイクル。
  4観点（Architecture、Test Strategy、API Design、Behavior）で最大3サイクル。
  「/review-fix」「レビュー修正」「実装後レビュー」「レビューフィックス」
  「修正サイクル」と依頼された時に使用。
model: opus
disable-model-invocation: true
---

# /review-fix

実装済みコードに対してレビュー・修正サイクルを実行し、品質を担保する。

## 役割: オーケストレーター

**自身の責務はオーケストレーションに専念。レビューを含むすべての実作業はサブエージェントに委譲する。**

### 役割分担

| 自身（オーケストレーター） | サブエージェント委譲 |
|--------------------------|-------------------|
| 対象ファイル特定 | レビュー（opus） |
| issue同一性判定 | コード修正（sonnet） |
| review-fix.md書き出し | Lint実行（haiku） |
| サイクル継続判断・報告 | テスト実行（sonnet） |

## 実行手順

### Step 1: 対象ファイル特定

以下の優先順で対象ファイルを取得:

1. `$ARGUMENTS` が指定されている場合 → そのパスを使用（override）
2. `git diff --name-only HEAD` と `git ls-files --others --exclude-standard` を併用して変更ファイル＋未追跡（新規作成）ファイルを取得（primary）
3. implementation.md のタスク対象ファイルを抽出（fallback）

**対象がない場合**: 中止し理由を報告。

**20ファイル超**: 20ファイルずつバッチに分割してStep 2以降を実行。

### Step 2: レビュー（サブエージェント委譲・opus）

**自分ではレビューしない。サブエージェントに委譲する。**

コンテキスト分離により、毎サイクル新鮮な視点でレビューする。

```
Task tool

subagent_type: general-purpose
model: opus
prompt: |
  あなたはコードレビューの専門家です。

  ## レビュー対象ファイル
  ${対象ファイルパスのリスト}

  ## レビュー観点（4観点）
  ${references/review-criteria.md の内容を展開}

  ## 高信号フィルタ

  フラグすべき:
  - コンパイル/パースエラー、型エラー
  - 明確なロジックエラー
  - セキュリティ脆弱性（SQLインジェクション、XSS等）
  - CLAUDE.md/rules違反（引用可能なもの）
  - テスト不足（重要パスのカバレッジ欠落）
  - API契約違反（戻り値型の不整合等）

  フラグしない:
  - コードスタイル（Linter管轄）
  - 潜在的問題（入力依存の仮定）
  - 既存コードの問題（新規導入ではない）
  - 命名の好み

  ## 出力形式

  各issueを以下の形式で出力:

  issue_id: REVIEW-${cycle}-{sequential}  # 例: REVIEW-1-003
  file: パス
  line: 行番号
  dimension: architecture | test_strategy | api_design | behavior
  problem: 問題の1行要約
  suggestion: 修正案

  issueがない場合は「指摘なし」とだけ出力。
```

### Step 3: review-fix.md 書き出し

`templates/output.md` の形式に従い、カレントディレクトリに `review-fix.md` として書き出す。

既存の `review-fix.md` がある場合は上書きする。

### Step 4: issue判定

- **issueなし** → Step 7（完了）へ
- **issueあり** → Step 5 へ
- **前サイクルと同一issue**（file+line一致） → `[RECURRING]` マーク付与、手動対応推奨として記録しStep 5では修正対象外

### Step 5: 修正サブエージェント（sonnet）

**自分では修正しない。すべてサブエージェントに委譲する。**

[RECURRING] マーク付きissueは修正対象から除外する。

異なるファイルへの修正は単一メッセージで並列起動:

```
Task tool × N（対象ファイル数）

subagent_type: general-purpose
model: sonnet
prompt: |
  あなたはコード修正の専門家です。

  ## 修正対象
  ファイル: ${file}

  ## issue一覧（このファイル分）
  ${該当ファイルのissue群}

  各issueについて:
  - issue_id: ${issue_id}
  - 行番号: ${line}
  - 観点: ${dimension}
  - 問題: ${problem}
  - 修正案: ${suggestion}

  ## 指示
  1. 該当箇所を特定し修正を実施
  2. 周辺コードとの整合性を確認
  3. 変更内容をissue_idごとに報告

  修正後、既存のテストを破壊しないこと。
```

**バッチ処理**:
- 7ファイル以下: 単一メッセージで並列起動
- 8ファイル以上: 7ファイルずつバッチ、完了待ち→次バッチ

### Step 6: 品質ゲート（サブエージェント委譲）

**自分では実行しない。サブエージェントに委譲する。**

#### 品質ゲートコマンドの特定

以下の優先順で検索:

1. implementation.md の品質ゲート定義
2. package.json の scripts（lint, test）
3. Makefile のターゲット（lint, test）
4. **該当なし → スキップ + 警告**: 「品質ゲートコマンドが見つかりません。Lint/Testは手動で実行してください。」

#### 実行

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

**失敗時**: 修正サブエージェントを起動して修正を委譲（Step 5と同じ要領）。
修正後、Step 2 へ戻る（次サイクル）。

**成功時**: Step 2 へ戻る（次サイクル）。

**最大サイクル到達（3回）**: Step 7 へ。

### Step 7: 完了報告

review-fix.md を最終更新し、結果を報告する。

#### issueゼロ時

```
## レビュー・修正完了

**レビュー**: issueゼロ
**修正サイクル**: X回

**品質ゲート**: 全クリア
```

#### 最大サイクル到達時

```
## レビュー・修正完了（警告あり）

**レビュー**: X件のissueが残存
**修正サイクル**: 3回（上限到達）

**残存issue**:
- `file.ts:42` - [問題の簡潔な説明]
- `file.ts:88` - [RECURRING] [問題の簡潔な説明]

**推奨アクション**:
- [RECURRING] issueは設計判断の見直しを検討
- 手動で残存issueを確認
```

## 制約

- 最大修正サイクル: 3回
- 同一issue（file+line一致）が再検出 → [RECURRING]マーク、修正対象外
- レビュー対象20ファイル超 → 20ファイルずつバッチ
- 修正対象8ファイル以上 → 7ファイルずつバッチ
