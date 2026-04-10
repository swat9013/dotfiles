---
name: implement
description: implementation.mdのタスクを品質ゲート付きで並列実行。Use when「/implement」「タスク実行」「実装して」「実装開始」。
model: sonnet
effort: medium
---

# /implement コマンド

implementation.md のタスクを Phase 別に実行する。並列実行可能タスク（Pマーク）は同時実行し、品質ゲートで各フェーズの品質を担保する。

## 二層構造

| 層 | 担当 | 役割 |
|----|------|------|
| implementation.md | 実装計画 | How（タスク定義・成功基準・依存関係。読み取り専用） |
| TaskCreate/TaskUpdate | 実行追跡 | Status（進捗管理・依存グラフ・UI可視化） |

implementation.md はタスクの仕様源として読み込むが、ステータス更新は行わない。実行追跡は Tasks API に一元化する。

**ワークフロー上の位置**:
```
plan.md (What/Why) → implementation.md (How) → TaskCreate (Status)
  architect が生成     breakdown が生成         implement が登録・実行
```

## 役割: オーケストレーター

**あなたはマネージャーであり、エージェントオーケストレーターです。**

### 絶対ルール

- **コードの実装・修正は絶対に自分でやらない**
- **テスト・Lint・ビルドの実行はスクリプト（quality-gate.py）で実行する**
- **AI判断を要する作業（レビュー、修正、設計判断）はサブエージェントに委譲する**

### 行動の3分類

| 分類 | 行動 | ツール |
|------|------|--------|
| **自身で行う** | implementation.md読込、タスク分析、TaskCreate登録、進捗監視、報告 | Read, TaskCreate, TaskUpdate, TaskList, TaskGet, 出力 |
| **スクリプト実行** | テスト実行、Lint、ビルド（品質ゲート） | Bash tool（quality-gate.py） |
| **サブエージェント委譲** | 実装、修正、レビュー | Task tool |
| **スキル委譲** | 複雑なサブワークフロー | Skill tool |

## 前提条件

以下を確認してから実行。満たさない場合は中止し理由を報告:

1. implementation.md が存在する
2. タスクに種別・成功基準が明記されている

## 実行手順

### 1. 前提条件チェック（自身で実行）

対象ファイルを以下の優先順で特定:
1. $ARGUMENTS 指定があればそのパスを使用
2. `.claude/implement/` を Glob し名前降順で先頭ファイルを使用
※ファイルが見つからない場合はエラーで止まり、breakdown スキルを先に実行するよう案内する

Read tool でファイルを読み込み、以下を確認:
- ファイルが存在するか
- タスクに種別・成功基準が明記されているか

**→ 問題があれば中止し理由を報告**

### 2. タスク登録（自身で実行）

implementation.md の内容から全タスクを TaskCreate で登録する。

**登録フロー**:
1. 全タスクを読み取り（ID、タイトル、種別、Phase、依存関係、並列可否）
2. 各タスクを TaskCreate で登録
3. Phase ごとの品質ゲートタスクを作成
4. 依存関係を TaskUpdate の `addBlockedBy` で設定

**TaskCreate 登録パターン**:

```
# 実装タスク
TaskCreate:
  subject: "TASK-001: ログインAPI実装"
  description: |
    種別: 新機能実装
    成功基準: ${implementation.mdの成功基準}
    制約: ${implementation.mdの制約}
  activeForm: "TASK-001 実装中"
  metadata: { phase: 1, type: "impl", taskId: "TASK-001" }

# 品質ゲートタスク
TaskCreate:
  subject: "Phase 1 品質ゲート"
  description: "Phase 1 の全タスク完了後に Lint/Test/Build を実行"
  activeForm: "Phase 1 品質チェック中"
  metadata: { phase: 1, type: "gate" }
```

**依存関係の設定**:

```
# 品質ゲートは Phase 内の全タスクに依存
TaskUpdate: { taskId: "gate-1", addBlockedBy: ["task-1", "task-2", ...] }

# 次 Phase のタスクは前 Phase の品質ゲートに依存
TaskUpdate: { taskId: "task-3", addBlockedBy: ["gate-1"] }
```

**subject 設計の注意**: TaskList は description を返さない。subject にタスクID・タイトルを含め、一覧だけで識別できるようにする。

### 3. Phase 別実行サイクル

```
TaskList → 実行可能タスク特定 → テスト先行チェック(3.5) → サブエージェント起動 → 完了待機
→ TaskUpdate(completed) → 品質ゲート自動アンブロック → 品質チェック委譲
→ Phase完了報告 → 次Phase
```

### 3.5. テスト先行チェック（自身で実行）

タスクを起動する前に確認:
- 「新機能実装」「リファクタリング」「バグ修正」種別タスクに、対応する「テスト追加」タスクが依存関係に存在するか
- 存在しない場合: ユーザーに確認する（意図的なテスト省略か？）
- テスト追加タスクが未完了の場合: 実装タスクを起動しない（依存関係として扱う）

注意: task-roles.md のサブエージェント向けロール指示とは責務が異なる。このステップはオーケストレーター層での実行順序制御に限定する。

### 4. タスク実行（すべてサブエージェントに委譲）

#### 並列実行（Pマーク付きタスク）

**単一メッセージで複数の Task tool を同時呼び出し**:

```
Task tool × N（並列実行可能タスク数、最大7）

subagent_type: general-purpose
model: sonnet  # タスク種別に応じて選択（guides/task-roles.md 参照）
prompt: |
  あなたは${ロール名}です。

  ## コンテキスト
  ${何を・なぜ — 背景と目的}

  ## タスク
  ID: ${task_id}
  タイトル: ${title}
  種別: ${type}

  ## 指示
  ${範囲・制約 — 何をやるか・やらないか}

  ## 関連ファイル
  ${具体的なファイルパスのリスト}

  ## 成功基準
  ${期待アウトプット形式}

  ## 完了報告
  以下を含めて報告:
  - 変更ファイル一覧
  - 変更内容の要約
  - 成功基準の充足状況
```

**委譲プロンプトの必須4要素**:

| 要素 | 内容 | 省略時のリスク |
|------|------|---------------|
| コンテキスト | 何を・なぜ | 誤った前提で実装 |
| 明示的な指示 | 範囲・制約 | スコープ超過 |
| 関連ファイルパス | 具体的パス | 無駄な探索 |
| 成功基準 | 期待アウトプット | 不完全な成果物 |

#### 順次実行（依存タスク）

DAG 依存により自動制御される。TaskList で blockedBy が空のタスクのみ実行対象。

#### 実行前後のステータス更新

```
TaskUpdate(taskId, status: "in_progress")  # 開始前
→ サブエージェント委譲
→ TaskUpdate(taskId, status: "completed")  # 成功時
# 失敗時は in_progress のまま維持し、修正サブエージェントを起動
```

### 5. 品質ゲート（スクリプト実行）

品質ゲートタスクが自動アンブロックされたら、Bash toolで直接実行:

```bash
# implementation.md にコマンド定義がある場合
~/.dotfiles/.claude-global/skills/scripts/quality-gate.py --lint-cmd="npm run lint" --test-cmd="npm test"

# 自動検出に任せる場合
~/.dotfiles/.claude-global/skills/scripts/quality-gate.py
```

出力JSONの `gate` フィールドで判定する。

**GATE: PASS**: 品質ゲートタスクを completed → 次Phase のタスクが自動アンブロック
**GATE: FAIL**: 修正サブエージェントを起動し、修正後にスクリプトを再実行

### 6. Phase完了報告

```markdown
## Phase X 完了報告

### 実施内容
- TASK-001: [タイトル]（完了）
- TASK-002: [タイトル]（完了）

### 品質ゲート結果
- Lint: 成功
- テスト: 全XX件通過

### 受け入れ条件（plan.md 由来）
- [x] [条件1] — TASK-001, TASK-002 の成功基準充足により達成
- [ ] [条件2] — [未充足理由と対応]

### 次Phase概要
- 並列実行可能タスク: X件
```

### 7. 最終レビューサイクル（最大3回）

全Phaseの品質ゲート通過後、コードレビュー・修正サイクルを実行する。

#### 7.1 対象ファイル特定（スクリプト実行）

```bash
~/.dotfiles/.claude-global/skills/scripts/changed-files.sh
```

`RESULT` 判定: `NO_CHANGES` / `CONFIG_ONLY` → スキップしStep 8へ。`TOO_LARGE` → 20ファイルずつバッチ。

#### 7.2 レビュー・修正サイクル

```
レビュー(opus) → issue判定 → 修正(sonnet) → 品質ゲート → 次サイクル or Step 8
```

**レビュー（サブエージェント委譲・opus）**:

```
Task tool

subagent_type: general-purpose
model: opus
prompt: |
  あなたはコードレビューの専門家です。

  ## レビュー対象ファイル
  ${対象ファイルパスのリスト}

  ## レビュー観点（4観点）
  ${_shared/review-criteria.md の内容を展開}

  ## 高信号フィルタ

  フラグすべき:
  - コンパイル/パースエラー、型エラー
  - 明確なロジックエラー
  - セキュリティ脆弱性（SQLインジェクション、XSS等）
  - CLAUDE.md/rules違反（引用可能なもの）
  - テスト不足（重要パスのカバレッジ欠落）
  - テストファースト違反（新規公開API/関数にテスト未作成）
  - API契約違反（戻り値型の不整合等）

  フラグしない:
  - コードスタイル（Linter管轄）
  - 潜在的問題（入力依存の仮定）
  - 既存コードの問題（新規導入ではない）
  - 命名の好み

  ## 出力形式

  各issueを以下の形式で出力:

  issue_id: REVIEW-${cycle}-{sequential}
  file: パス
  line: 行番号
  dimension: architecture | test_strategy | api_design | behavior
  problem: 問題の1行要約
  suggestion: 修正案

  issueがない場合は「指摘なし」とだけ出力。
```

**issue判定（自身で実行）**:
- issueなし → Step 8 へ
- 前サイクルと同一issue（file+line一致） → `[RECURRING]` マーク、修正対象外
- issueあり → 修正へ

**修正（サブエージェント委譲・sonnet）**:

異なるファイルへの修正は単一メッセージで並列起動（最大7）:

```
Task tool × N

subagent_type: general-purpose
model: sonnet
prompt: |
  あなたはコード修正の専門家です。

  ## 修正対象
  ファイル: ${file}

  ## issue一覧（このファイル分）
  ${該当ファイルのissue群}

  ## 指示
  1. 該当箇所を特定し修正を実施
  2. 周辺コードとの整合性を確認
  3. 変更内容をissue_idごとに報告

  修正後、既存のテストを破壊しないこと。
```

**品質ゲート**: quality-gate.py 実行。FAIL → 修正サブエージェント起動後にサイクル先頭へ。

**最大3サイクル到達**: 残存issueを報告しStep 8 へ。

### 8. レビュー結果ファイル書き出し

レビューサイクルを実行した場合、結果を `.claude/tmp/review/YYYY-MM-DD-HHMMSS-{topic}.md` に書き出す。
- `{topic}`: 変更ファイル群から推定したkebab-caseスラッグ

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
| フロントエンド/UI実装 | frontend-design/SKILL.md参照、独自性、意図的な美的選択 |

## 成功基準

このコマンドの実行は以下を満たしたとき成功とみなす:

1. TaskList で全タスクが completed ステータスになっている
2. 全 Phase の品質ゲートをクリアしている
3. 最終レビューサイクルを完了している（issueゼロ or 最大3サイクル到達）
4. implementation.md に記載された成功基準を満たしている
5. implementation.md の各 Phase 受け入れ条件（plan.md 由来）が全充足されている

## 完了チェックリスト

- [ ] 全Phaseのタスクが completed（TaskList で確認）
- [ ] 全品質ゲートをクリア
- [ ] 最終レビューサイクル完了（レビュー結果ファイル書き出し済み）
- [ ] implementation.md の成功基準を満たしている
- [ ] 各 Phase の受け入れ条件が全充足されている

## Gotchas

- **TaskList は description を返さない**: subject にタスクID・タイトルを含め、一覧で識別可能にする
- **サブエージェントは孫エージェントをスポーンできない**: 1段階のみ。複雑なサブワークフローは Skill 委譲で対応
- **同時サブエージェント上限**: ~7。超過分は次のバッチで実行
- **Task* ツールは hooks をバイパス**: PreToolUse/PostToolUse フックが適用されない
- **失敗時のステータス**: completed にしない。in_progress のまま維持し修正を試みる

## 出力形式

```
## 実装完了

**完了タスク**: X個
**品質ゲート**: 全クリア
**レビュー**: issueゼロ（Y回サイクル） / X件残存（3回上限到達）

**残存issue**（ある場合）:
- `file.ts:42` - [問題の簡潔な説明]
- `file.ts:88` - [RECURRING] [問題の簡潔な説明]

**次のステップ**:
- PR作成
```
