---
name: implement
description: implementation.mdのタスクを並列実行。品質ゲート付きでPhase別に実行。「/implement」「タスク実行」「実装して」「実装開始」「実装を進めて」「タスクを実行して」「並列実装」「実装フェーズ」「タスク消化」と依頼された時に使用。
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
- **テスト・Lint・ビルドの実行はスクリプト（quality-gate.sh）で実行する**
- **AI判断を要する作業（レビュー、修正、設計判断）はサブエージェントに委譲する**

### 行動の3分類

| 分類 | 行動 | ツール |
|------|------|--------|
| **自身で行う** | implementation.md読込、タスク分析、TaskCreate登録、進捗監視、報告 | Read, TaskCreate, TaskUpdate, TaskList, TaskGet, 出力 |
| **スクリプト実行** | テスト実行、Lint、ビルド（品質ゲート） | Bash tool（quality-gate.sh） |
| **サブエージェント委譲** | 実装、修正 | Task tool |
| **スキル委譲** | 複雑なサブワークフロー | Skill tool |

## 前提条件

以下を確認してから実行。満たさない場合は中止し理由を報告:

1. implementation.md が存在する
2. タスクに種別・成功基準が明記されている

## 実行手順

### 1. 前提条件チェック（自身で実行）

Read tool で implementation.md を読み込み、以下を確認:
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
TaskList → 実行可能タスク特定 → サブエージェント並列起動 → 完了待機
→ TaskUpdate(completed) → 品質ゲート自動アンブロック → 品質チェック委譲
→ Phase完了報告 → 次Phase
```

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
~/.dotfiles/.claude-global/skills/scripts/quality-gate.sh --lint-cmd="npm run lint" --test-cmd="npm test"

# 自動検出に任せる場合
~/.dotfiles/.claude-global/skills/scripts/quality-gate.sh
```

出力の `GATE: PASS/FAIL` で判定する。

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
| フロントエンド/UI実装 | frontend-design/SKILL.md参照、独自性、意図的な美的選択 |

## 成功基準

このコマンドの実行は以下を満たしたとき成功とみなす:

1. TaskList で全タスクが completed ステータスになっている
2. 全 Phase の品質ゲートをクリアしている
3. implementation.md に記載された成功基準を満たしている

## 完了チェックリスト

- [ ] 全Phaseのタスクが completed（TaskList で確認）
- [ ] 全品質ゲートをクリア
- [ ] implementation.md の成功基準を満たしている

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

**次のステップ**:
- /code-review でコードレビュー
- PR作成
```
