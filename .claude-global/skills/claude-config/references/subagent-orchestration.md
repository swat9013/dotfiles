# サブエージェントオーケストレーション リファレンス

## TOC
1. [Agent tool パラメータ・呼び出し](#agent-tool-パラメータ呼び出し)
2. [subagent_type 一覧](#subagent_type-一覧)
3. [モデル選択ガイド](#モデル選択ガイド)
4. [run_in_background 制約](#run_in_background-制約)
5. [isolation: "worktree" 仕様](#isolation-worktree-仕様)
6. [カスタムエージェント frontmatter 完全仕様](#カスタムエージェント-frontmatter-完全仕様)
7. [permissionMode 6種類](#permissionmode-6種類)
8. [既知の制約](#既知の制約)

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

| タスク種別 | モデル | 判断基準 |
|-----------|--------|---------|
| ファイル検索、前提条件チェック、定型出力収集 | `haiku` | 決定論的・高速（Opusの1/10コスト） |
| サマリー生成、コンプライアンスチェック | `sonnet` | バランス型 |
| バグ検出、セキュリティ分析、複雑な推論 | `opus` | 精度優先 |

- 親セッションのデフォルトモデルを引き継ぐ場合は `inherit`（省略時も同じ）
- 検証サブエージェントはバグ検出と同じモデルを使う
- 同じ観点で複数並列させる場合は同モデルで統一
- **推論サンドイッチ**: 計画+検証フェーズに高推論（opus/high effort）、実装フェーズに中推論（sonnet/medium effort）を割り当てると精度と効率のバランスが取れる

---

## run_in_background 制約

| 項目 | フォアグラウンド | バックグラウンド |
|------|----------------|----------------|
| 権限プロンプト | 実行中に表示 | 事前承認が必要（なければ失敗） |
| MCP ツール | 利用可 | **利用不可** |
| TaskOutput での取得 | 即時 | `block: true` で完了待機 |
| 推奨同時実行数 | 5-7個 | 3-5個 |
| 用途 | 短時間・インタラクティブ | 長時間・CPU集約型・完全自律 |

`run_in_background` 使用時は MCP ツール（glab, gh, etc.）が使えない。CLI ツールは Bash 経由で代替。

> **v2.1.83**: `TaskOutput` ツールは**非推奨化**。バックグラウンドタスクの出力はファイルパスへの `Read` で取得する。フォアグラウンドで最大5並列が安全上限。

---

## isolation: "worktree" 仕様

```yaml
Agent tool:
- isolation: "worktree"
- run_in_background: true
- prompt: authentication モジュールをリファクタリング...
```

- `.claude/worktrees/` 以下に一時的な git worktree を作成（HEAD ベース）
- 複数エージェントが同一ファイルを修正しても競合しない
- 変更がない場合は実行後に自動削除、変更があった場合はブランチ名と worktree パスが返される
- クラッシュで孤立した worktree は `cleanupPeriodDays` 設定後に自動削除
- 非 Git VCS（SVN/Perforce 等）では `WorktreeCreate` / `WorktreeRemove` hooks で代替可能

使用場面: 複数エージェントが同一リポジトリで独立した機能を並列実装する場合。

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
| `effort` | - | enum | `low` / `medium` / `high` / `max`（`max` は extended thinking 最大活用、主に Opus 系で効果大） |
| `color` | - | enum | UI表示色: `red`/`blue`/`green`/`yellow`/`purple`/`orange`/`pink`/`cyan` |
| `initialPrompt` | - | string | `--agent` 起動時の自動サブミットプロンプト |
| `skills` | - | array | 起動時コンテキスト注入スキル（明示列挙必須） |
| `mcpServers` | - | object | 使用可能な MCP サーバー |
| `hooks` | - | object | スコープ付きhooks（PreToolUse, PostToolUse, Stop） |
| `memory` | - | enum | `user` / `project` / `local` |
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

> レビュー・出力パターン（高信号フィルタリング基準、出力形式標準化、検証パターン）は `review-patterns.md` に分離。

---

## 既知の制約

既知の制約は `rules/claude-global-skills.md` の Gotchas セクションを参照。Agent Teams 固有の制限は `agent-teams.md` 参照。
