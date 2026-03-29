# サブエージェントオーケストレーション リファレンス

## TOC
1. [Agent tool パラメータ全量](#agent-tool-パラメータ全量)
2. [subagent_type 一覧](#subagent_type-一覧)
3. [モデル選択ガイド](#モデル選択ガイド)
4. [run_in_background 制約](#run_in_background-制約)
5. [isolation: "worktree" 仕様](#isolation-worktree-仕様)
6. [カスタムエージェント frontmatter 完全仕様](#カスタムエージェント-frontmatter-完全仕様)
7. [permissionMode 5種類](#permissionmode-5種類)
8. [高信号フィルタリング基準](#高信号フィルタリング基準)
9. [出力形式標準化](#出力形式標準化)
10. [既知の制約](#既知の制約)

---

## Agent tool パラメータ全量

| パラメータ | 必須 | 型 | デフォルト | 説明 |
|-----------|------|-----|----------|------|
| `subagent_type` | ✓ | string | - | エージェントタイプ |
| `prompt` | ✓ | string | - | タスク内容 |
| `description` | 推奨 | string | - | 3-5語のタスク説明（ログ・進捗表示用） |
| `model` | - | enum | `inherit` | `sonnet` / `opus` / `haiku` / `inherit` |
| `run_in_background` | - | boolean | `false` | 非同期実行 |
| `isolation` | - | string | なし | `"worktree"` で git worktree 内で実行 |
| `resume` | - | string | - | 既存セッション ID を指定して再開 |
| `max_turns` | - | number | 設定依存 | 最大ターン数 |

---

## subagent_type 一覧

| タイプ | 利用可能ツール | 用途 |
|--------|--------------|------|
| `general-purpose` | 全ツール | 複雑な多段階タスク、探索+修正 |
| `Explore` | 読み取り専用（Read, Glob, Grep 等） | コードベース探索・検索（高速・低コスト） |
| `Plan` | 読み取り専用 | プランモードでの調査 |
| `Bash` | Bash のみ | コマンド実行（独立コンテキスト） |
| `claude-code-guide` | WebFetch, WebSearch, Read 等 | ビルトイン: Claude Code 機能Q&A |
| `statusline-setup` | Read, Edit | ビルトイン: `/statusline` 設定 |
| カスタム名 | frontmatter `tools` 設定次第 | `.claude/agents/` または `~/.claude/agents/` で定義 |

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

---

## run_in_background 制約

| 項目 | フォアグラウンド | バックグラウンド |
|------|----------------|----------------|
| 権限プロンプト | 実行中に表示 | 事前承認が必要（なければ失敗） |
| MCP ツール | 利用可 | **利用不可** |
| TaskOutput での取得 | 即時 | `block: true` で完了待機 |
| 推奨同時実行数 | 5-7個 | 3-5個 |
| 用途 | 短時間・インタラクティブ | 長時間・CPU集約型・完全自律 |

`run_in_background: true` + `TaskOutput` 並列取得は全失敗（Sibling error）。フォアグラウンドで最大5並列が安全上限。
`run_in_background` 使用時は MCP ツール（glab, gh, etc.）が使えない。CLI ツールは Bash 経由で代替。

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
| `permissionMode` | - | enum | 下記5種参照 |
| `maxTurns` | - | number | 最大ターン数 |
| `effort` | - | enum | effort レベル（`low` / `medium` / `high`） |
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

エージェントスコープ優先度: `--agents` CLI引数 > `.claude/agents/` > `~/.claude/agents/`

---

## permissionMode 5種類

| モード | 動作 | 用途 |
|--------|------|------|
| `default` | 標準的な権限チェック | 汎用 |
| `acceptEdits` | ファイル編集を自動承認 | コード修正エージェント |
| `dontAsk` | 権限プロンプトを自動拒否 | 読み取り専用分析 |
| `bypassPermissions` | 全権限チェックをスキップ | CI/CD等の信頼環境のみ |
| `plan` | 読み取り専用（プランモード） | 計画策定エージェント |

`bypassPermissions` は信頼できる自動化環境のみ使用。ローカル開発での使用は避ける。

---

## 高信号フィルタリング基準

| フラグすべき（高信号） | フラグしない（偽陽性回避） |
|---------------------|------------------------|
| コンパイル/パースエラー | コードスタイルの懸念（Linter がカバー） |
| 型エラー、missing imports | 入力値依存の潜在的問題 |
| 明白なロジックエラー（引用可能なコード行あり） | 主観的な改善提案 |
| CLAUDE.md / rules に明記された違反（引用可能） | 確実でない問題（「かもしれない」レベル） |
| セキュリティ欠陥（SQLインジェクション、XSS、認証不備） | 既存コードに存在する問題（新規導入ではない） |

**判断テスト**: 「このissueを GitHub PR コメントに書いて恥ずかしくないか？」

**検証サブエージェントパターン**:
1. Agent A: issue 検出（並列）
2. Agent B: 各issueの妥当性を個別検証（並列）
3. 検証されなかったissueは破棄

---

## 出力形式標準化

**GitHub リンク形式**: `https://github.com/owner/repo/blob/[FULL_SHA]/path/to/file.ts#L42-L55`
- フル git SHA 必須（短縮形 NG）、行範囲: `#L[start]-L[end]`、対象行の前後1行以上を含める

**コメントサイズ別**:

| サイズ | 方針 |
|--------|------|
| 6行以下 | 修正提案コードブロックを含める |
| 6行超 | 説明のみ、修正提案なし |
| 1 issue | 1 comment（重複禁止） |

**サブエージェントへのルール継承**: サブエージェントは親のコンテキストを継承しない。prompt に必要なルールを明示:

```markdown
## 全エージェント共通ルール
- フラグすべき: コンパイルエラー、型エラー、明白なロジックバグ
- フラグしない: スタイル問題、潜在的な問題、確実でないもの
- 出力形式: | ファイル | 行 | 種別 | 説明 |
- 1 issue = 1 行（重複禁止）
```

---

## 既知の制約

| 制約 | 詳細 |
|------|------|
| 孫エージェントスポーン | Agent tool は1段階のみ（孫エージェント起動不可） |
| PreToolUse/PostToolUse hooks | サブエージェント内でバイパスされる（frontmatter `hooks` で部分対応可。plugin subagent は hooks 無視） |
| スキル間参照 | 自動読み込みされない。references/ は同一スキル内のみ機能 |
| Haiku での ToolSearch | `tool_reference blocks` 非対応（使用不可） |
| run_in_background + TaskOutput 並列 | Sibling error で全失敗。フォアグラウンド5並列が上限 |
| セッション再開 | `resume` パラメータでセッション ID 指定。チームメイトは再開不可 |
