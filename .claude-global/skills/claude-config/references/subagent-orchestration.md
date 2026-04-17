# サブエージェントオーケストレーション リファレンス

## TOC
1. [Agent tool パラメータ・呼び出し](#agent-tool-パラメータ呼び出し)
2. [subagent_type 一覧](#subagent_type-一覧)
3. [モデル選択ガイド](#モデル選択ガイド)
4. [run_in_background 制約](#run_in_background-制約)
5. [isolation: "worktree" 仕様](#isolation-worktree-仕様)
6. [カスタムエージェント frontmatter 完全仕様](#カスタムエージェント-frontmatter-完全仕様)
7. [permissionMode 6種類](#permissionmode-6種類)
8. [委譲判断基準](#委譲判断基準)
9. [ワークフローパターン](#ワークフローパターン)
10. [アンチパターン](#アンチパターン)
11. [既知の制約](#既知の制約)

---

## Agent tool パラメータ・呼び出し

| パラメータ | 必須 | 型 | デフォルト | 説明 |
|-----------|------|-----|----------|------|
| `subagent_type` | ✓ | string | - | エージェントタイプ |
| `prompt` | ✓ | string | - | タスク内容 |
| `description` | 推奨 | string | - | 3-5語のタスク説明（ログ・進捗表示用） |
| `model` | - | enum | `inherit` | `sonnet` / `opus` / `haiku` / `inherit` |
| `run_in_background` | - | boolean | `false` | 非同期実行 |
| `isolation` | - | string | なし | `"worktree"` で git worktree 内で実行 |

**モデル解決優先順位**: ① 環境変数 `CLAUDE_CODE_SUBAGENT_MODEL` → ② Agent tool `model` パラメータ → ③ frontmatter `model` → ④ メインセッションのモデル（inherit）

**呼び出し方法**:

| 方法 | 例 | 特徴 |
|------|-----|------|
| 自然言語 | `Use the code-reviewer subagent to...` | Claude が判断して委譲 |
| @メンション | `@"code-reviewer (agent)" look at...` | 確実に指定エージェントを呼ぶ |
| セッション全体 | `claude --agent code-reviewer` | メインスレッド自体がそのエージェントになる |

settings.json `"agent": "code-reviewer"` でデフォルト化可能。

**Resume**: `SendMessage` ツールでエージェント ID を指定して再開（`CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1` 時のみ）。各起動は新インスタンス（フレッシュコンテキスト）。

---

## subagent_type 一覧

| タイプ | モデル | 利用可能ツール | 用途 |
|--------|--------|--------------|------|
| `general-purpose` | 継承 | 全ツール | 複雑な多段階タスク、探索+修正 |
| `Explore` | Haiku | 読み取り専用（Read, Glob, Grep 等） | コードベース探索・検索（高速・低コスト） |
| `Plan` | 継承 | 読み取り専用 | プランモードでの調査 |
| `claude-code-guide` | Haiku | WebFetch, WebSearch, Read 等 | ビルトイン: Claude Code 機能Q&A |
| `statusline-setup` | Sonnet | Read, Edit | ビルトイン: `/statusline` 設定 |
| カスタム名 | frontmatter 依存 | frontmatter `tools` 設定次第 | `.claude/agents/` または `~/.claude/agents/` で定義 |

**スコープ優先度**: Managed settings > `--agents` CLI > `.claude/agents/` > `~/.claude/agents/` > Plugin `agents/`

---

## モデル選択ガイド

CLAUDE.md の基本方針（基本 sonnet、軽量タスクのみ haiku）に加えて:

- **推論サンドイッチ**: 計画+検証フェーズに高推論（opus/high effort）、実装フェーズに中推論（sonnet/medium effort）を割り当てると精度と効率のバランスが取れる
- 検証サブエージェントは検出対象と同じモデルを使う
- 同じ観点で複数並列させる場合は同モデルで統一

---

## run_in_background 制約

| 項目 | フォアグラウンド | バックグラウンド |
|------|----------------|----------------|
| 権限プロンプト | 実行中に表示 | 事前承認が必要（なければ失敗） |
| MCP ツール | 利用可 | **利用不可** |
| 出力取得 | 即時 | ファイルパスへの `Read` で取得（`TaskOutput` は v2.1.83 で非推奨） |
| 推奨同時実行数 | 5-7個 | 3-5個 |
| 用途 | 短時間・インタラクティブ | 長時間・CPU集約型・完全自律 |

`run_in_background` 使用時は MCP ツール（glab, gh, etc.）が使えない。CLI ツールは Bash 経由で代替。フォアグラウンドで最大5並列が安全上限。

---

## isolation: "worktree" 仕様

- `.claude/worktrees/` 以下に一時的な git worktree を作成（HEAD ベース）
- 複数エージェントが同一ファイルを修正しても競合しない
- 変更なし→自動削除、変更あり→ブランチ名と worktree パスが返される
- 孤立 worktree は `cleanupPeriodDays` 後に自動削除
- 非 Git VCS では `WorktreeCreate` / `WorktreeRemove` hooks で代替可能

---

## カスタムエージェント frontmatter 完全仕様

配置場所: `.claude/agents/<name>.md`（プロジェクト共有）または `~/.claude/agents/<name>.md`（全プロジェクト）

| フィールド | 必須 | 型 | 説明 |
|-----------|------|-----|------|
| `name` | ✓ | string | 小文字とハイフンのみ。`Agent(name)` で参照 |
| `description` | ✓ | string | 自動選択の判断材料（キーワード重要） |
| `tools` | - | string | 許可ツールリスト（省略時: 全ツール継承） |
| `disallowedTools` | - | string | 拒否ツールリスト |
| `model` | - | enum | `sonnet` / `opus` / `haiku` / `inherit` |
| `permissionMode` | - | enum | 下記6種参照 |
| `maxTurns` | - | number | 最大ターン数 |
| `effort` | - | enum | `low` / `medium` / `high` / `xhigh` / `max`。利用可能レベルはモデル依存（`xhigh` は Opus 4.7 のみ。`max` は主に Opus 系で extended thinking 最大活用） |
| `color` | - | enum | UI表示色: `red`/`blue`/`green`/`yellow`/`purple`/`orange`/`pink`/`cyan` |
| `initialPrompt` | - | string | `--agent` 起動時の自動サブミットプロンプト |
| `skills` | - | array | 起動時コンテキスト注入スキル（明示列挙必須） |
| `mcpServers` | - | object | MCP サーバー。インライン定義=エージェント専用、文字列参照=既存接続共有 |
| `hooks` | - | object | スコープ付きhooks（PreToolUse, PostToolUse, Stop） |
| `memory` | - | enum | `user`(~/.claude/agent-memory/) / `project`(.claude/agent-memory/, 推奨) / `local`(.claude/agent-memory-local/) |
| `background` | - | boolean | `true` で常にバックグラウンド実行 |
| `isolation` | - | string | `"worktree"` で一時 worktree 内で実行 |

- 組み込みエージェント（Explore, Plan, general-purpose）は `skills` フィールドを持たない
- 親セッションのスキルは継承されない。必要なスキルは明示的にリスト

**サブエージェント生成制限**:
```yaml
tools: Agent(worker, researcher), Read, Bash  # worker と researcher のみ生成可
tools: Agent   # 全サブエージェント許可
# Agent 省略: サブエージェント生成禁止
```

> **v2.1.63+**: Task tool は Agent tool にリネーム。`Task(...)` はエイリアスとして動作するが、新規設定では `Agent(...)` を使用。

**description 自動選択キーワード**:
```yaml
description: Use PROACTIVELY when code is written or modified.
description: MUST BE USED for all security-related code reviews.
```

**`permissions.deny` で個別無効化**: `["Agent(Explore)", "Agent(my-custom-agent)"]`

---

## permissionMode 6種類

| モード | 動作 | 用途 |
|--------|------|------|
| `default` | 標準的な権限チェック | 汎用 |
| `acceptEdits` | ファイル編集を自動承認 | コード修正エージェント |
| `auto` | 全操作を自動承認（対話的確認なし） | 自動化パイプライン |
| `dontAsk` | 権限プロンプトを自動拒否 | 読み取り専用分析 |
| `bypassPermissions` | 全権限チェックをスキップ | CI/CD等の信頼環境のみ |
| `plan` | 読み取り専用（プランモード） | 計画策定エージェント |

`bypassPermissions` / `auto` は信頼できる自動化環境のみ使用。ローカル開発での使用は避ける。

**親セッションの permissionMode が優先されるケース**:
- 親が `bypassPermissions` または `acceptEdits` の場合、サブエージェント frontmatter の `permissionMode` は**上書きされず無視される**（親が優先）
- 親が `auto` モードの場合も同様。サブエージェントは `auto` を継承し、`permissionMode` は無視される。親の block/allow ルールがサブエージェントのツール呼び出しにも適用される

> レビュー・出力パターン（高信号フィルタリング基準、出力形式標準化、検証パターン）は `review-patterns.md` に分離。

---

## 委譲判断基準

| 状況 | 選択肢 | 理由 |
|------|--------|------|
| 大量出力を生成するタスク（テスト結果・ログ・ドキュメント取得） | Subagent | メイン会話のコンテキスト汚染を防止 |
| ツール制限・権限制御を強制したい | Subagent | `tools` / `disallowedTools` で最小権限を実現 |
| 独立した複数の調査パスを並列実行 | Subagent（並列） | 各パスが自己完結し結果サマリで十分 |
| 頻繁な往復・反復改善が必要 | メイン会話 | Subagent はコンテキスト収集のオーバーヘッドがある |
| 複数フェーズが重要コンテキストを共有（計画→実装→テスト） | メイン会話 | コンテキスト分断のコスト > 委譲のメリット |
| 小さなピンポイント変更 | メイン会話 | Subagent の起動コストに見合わない |
| メイン会話コンテキストで再利用可能なプロンプト/ワークフロー | Skills | Subagent の独立コンテキストは不要 |
| Teammate 間で情報共有・相互検証・自律協調が必要 | Agent Teams | Subagent は単方向報告のみ |

**選択フレームワーク**: 単一会話 → Skills → Subagents → Agent Teams の順で検討。最もシンプルなパターンから始める。

---

## ワークフローパターン

- **Sequential**: 各ステップが前の出力に依存する場合。アンチパターン: 独立可能なタスクを無駄に逐次化
- **Parallel**: 独立サブタスクを同時実行・集約。**必須**: 集約戦略を実装前に設計。アンチパターン: 累積コンテキストが必要な場合
- **Evaluator-Optimizer**: Generator → Evaluator ループ。**必須**: 最大反復回数と品質閾値を事前定義。アンチパターン: 評価基準が主観的、初回品質で十分な場合

---

## アンチパターン

| パターン | 問題 | 対策 |
|---------|------|------|
| description が曖昧 | 自動委譲が機能しない | 具体的トリガー条件を記述（`Use proactively after code changes`） |
| 多数の Subagent が詳細な結果を返す | メイン会話のコンテキスト消費 | サマリのみ返すよう prompt に明示 |
| 逐次化すべき作業を並列化 | 依存関係エラー・不整合 | 依存関係を明確化してから並列化判断 |
| ネスト 3 レベル超 | 管理困難・効率低下 | Skills でメイン会話ベースに変更 |
| `bypassPermissions` の安易な使用 | セキュリティリスク | `auto` mode を検討 |

---

## 既知の制約

既知の制約は `rules/claude-global-skills.md` の Gotchas セクションを参照。Agent Teams 固有の制限は `agent-teams.md` 参照。
