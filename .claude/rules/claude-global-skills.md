---
paths: .claude-global/skills/**
---

# グローバル Skills ルール

## 制約

| 項目 | 値 |
|------|-----|
| SKILL.md | 500行以下 |
| コンテンツ予算 | 15,000文字（スキル全体） |
| Progressive Disclosure | 詳細は `references/` に分離（1階層まで） |
| 100行超のreferences | 先頭にTOC必須 |

## frontmatter

命名規則: kebab-case（全体）、64文字以内（frontmatter name）

| フィールド | 必須 | 説明 |
|-----------|-----|------|
| `name` | ○ | gerund形式推奨（`processing-pdfs`等） |
| `description` | ○ | 三人称、トリガーキーワード必須 |
| `argument-hint` | - | 引数のヒント表示 |
| `disable-model-invocation` | - | `true`: Claude自動呼び出し無効 |
| `user-invocable` | - | `false`: ユーザー手動呼び出し無効 |
| `allowed-tools` | - | 使用可能ツールの制限 |
| `model` | - | 使用モデル指定（`opus`/`sonnet`/`haiku`） |
| `effort` | - | 思考深度指定（`low`/`medium`/`high`） |
| `context` | - | `fork`: コンテキスト分離 |
| `agent` | - | `true`: サブエージェントとして実行 |
| `hooks` | - | スキル固有のhooks設定 |

## description設計

**フォーマット**: `Verb+what. Use when+trigger1, trigger2, or trigger3.`

- **130文字以下**に収める（budget効率: 42→60+スキル収容可能）
- **三人称動詞で開始**（Analyzes, Generates等）。一人称（I analyze）・二人称（You can）は禁止
- 「Use when」以降にトリガーキーワードを**3〜5個**列挙（過多は逆効果）
- 曖昧な表現（「いろいろな」「便利な」）は避ける

## 呼び出し制御

| 設定 | ユーザー | Claude自動 | 用途 |
|------|---------|-----------|------|
| デフォルト | ○ | ○ | 汎用 |
| `disable-model-invocation: true` | ○ | ✕ | deploy, commit等の副作用あり |
| `user-invocable: false` | ✕ | ○ | 背景知識・参照ガイド |

## skills vs agents

| skills | agents |
|--------|--------|
| コンテキスト共有 | コンテキスト分離 |
| 手順定義、ガイド | 並列実行、専門役割 |

> `.claude/agents/` は使用しない。サブエージェントはskill内でAgent toolを呼び出す

## 優先度

Enterprise > Personal (`~/.claude/skills/`) > Project (`.claude/skills/`) > Plugin

**skills と commands が同名の場合、skillsが優先**

## 高度な機能

### 文字列置換

| 変数 | 内容 |
|------|------|
| `$ARGUMENTS` | 呼び出し時の引数全体 |
| `$ARGUMENTS[0]`, `[1]` | 位置指定アクセス |
| `${CLAUDE_SESSION_ID}` | セッションID |

### 動的コンテキスト注入

`` !`git branch --show-current` `` — スキルロード時にコマンド出力を展開

### Extended Thinking

SKILL.md内に `ultrathink` を含めると拡張思考モード有効化

### コンテキスト分離

```yaml
context: fork
model: sonnet  # モデル指定（省略時は親セッションのモデルを継承）
```

fork はサブエージェントとして隔離実行。メインコンテキストにはサマリのみ返る。

**致命的制約: fork内からAgent/Task/Skill toolは使用不可**（孫スポーン不可）。内部でサブエージェントを生成するスキルには適用できない。
**fork 適性**: サブエージェント不使用 + コンテキスト消費大 + ファイル契約で入出力が繋がる。**不適**: 内部Agent/Task tool使用 / 対話型 / 直前の会話を分析対象とする

### On Demand Hooks

frontmatter `hooks:` でスキル呼び出し時のみ有効なセッションスコープhooksを定義。常時有効だと邪魔だが特定作業時に必須のガードレールに最適（例: `/careful` で破壊的操作ブロック）

### データ永続化

`${CLAUDE_PLUGIN_DATA}`: プラグインごとの安定ストレージパス。スキルディレクトリ内のデータはアップグレード時に消えるため、永続データはこのパスに保存

## Progressive Disclosure

- SKILL.md本体にワークフロー/概要のみ
- 詳細は `references/` に分離（**1階層まで**）
- 100行超のreferencesは**先頭にTOC必須**
- 2つ以上のスキルで共有するreferences → `_shared/` に配置
- 設計注意事項の詳細（スキルリネーム・references記述基準・手順記述スタイル・スクリプト設計・model/effort等）は `references/skill-design-patterns.md` 参照

## Gotchas

- **サブエージェント残存**: Agent toolで起動したサブエージェントは親セッション終了後も残存しメモリ消費。定期的に`cck --sub`でクリーンアップ
- **Agent tool 並列制限**: `TaskOutput` は非推奨（v2.1.83）。バックグラウンドタスクの出力は `Read` でファイルパスから取得。foreground で最大5並列が安全上限
- **Agent tool 制約**: 孫エージェントスポーン不可（1段階のみ）。PreToolUse/PostToolUse hooks はバイパスされる（frontmatter `hooks` で部分対応可）。**plugin subagents は `hooks`/`mcpServers`/`permissionMode` frontmatter を無視（仕様確定）** — `.claude/agents/` にコピーで回避
- **Skill tool 連鎖不可**: Skill tool で他スキルを呼び出すと失敗。スキル間連携はユーザーに次スキルを案内
- **スキル間参照**: 自動読み込みされない。references/ は同一スキル内のみ機能。スキル間参照はデッドリンク化
- **ToolSearch**: claude-haiku-4-5 では利用不可（tool_reference blocks 非対応）
- **スキル内 `!` バッククォート**: インライン実行で失敗時エラー停止。`|| true` や `|| echo "(なし)"` でフォールバック必要

## アンチパターン

| パターン | 対策 |
|---------|------|
| 500行超 | references/に分離 |
| 15,000文字超 | 圧縮またはreferences/に分離 |
| 100行超referencesにTOCなし | 先頭にTOC追加 |
| トリガーなしdescription | 自動選択されない→トリガー追加 |
| 汎用description | 誤マッチ→具体的に |
| 深い参照ネスト | 1階層までに制限 |

## スキル設計意図の自己記述原則

スキルの設計意図（目的関数・サブエージェント配置判断）はSKILL.md自体に記述する。外部ドキュメント参照のみでは設計意図が不可視になり、維持・診断が困難になる。

### 目的関数の記述位置

親スキルと子サブエージェントで主目的関数が異なる場合（**親子目的関数非対称配置**）、その切り替え理由をSKILL.md内に明記する。記述なしは「意図なき実装」とみなす。

### 目的関数配置ルール表

| 階層 | 配置先 | 根拠 |
|------|-------|------|
| ドメイン目的関数（三項組） | `docs/harness-engineering-domain-model.md §2.x` | 正本一元化 |
| スキル固有 sub-objective | 各 `SKILL.md` 冒頭 | skill独立型 |
| 静的サブエージェント目的関数 | 各サブエージェント定義ファイル | 静的定義の自己完結 |
| 動的サブエージェント目的関数 | 親スキル `SKILL.md` 内 | 親との結合度が高い |
| 独立レビュー目的関数 | `_shared/independent-review-prompt.md` | 集約ハブ（ハブ経由注入） |

**配置先決定原則**: 観測単位に従う（観測粒度軸の再帰適用）

### 静的/動的サブエージェント目的関数配置判定ルール

- **静的サブエージェント**（定義ファイルあり、反復利用）: 目的関数は静的定義ファイル内に記述し、親スキル SKILL.md は呼び出し参照のみとする
- **動的サブエージェント**（親が実行時prompt生成）: 親スキル SKILL.md 内に記述し、静的定義ファイルの作成は禁止
- **切り替え判定**: 静的定義昇格原則を適用（反復利用が検出された時点で静的化）

## スキル一覧

開発サイクル全体像・スキル間のコナセンス → `rules/dev-cycle.md`
設定管理系のコナセンス → `rules/config-management.md`
### ワークフロー系スキル
| スキル | トリガー |
|--------|---------|
| architect | 「設計して」「plan.md作成」 |
| plan | 「/plan」「実装計画」「どう実装する？」 |
| breakdown | 「/breakdown」「タスク分解」 |
| implement | 「/implement」「タスク実行」 |
| review-fix | 「/review-fix」「レビュー修正」「実装後レビュー」 |
| refactor | 「リファクタリング」「コード改善」 |
| researcher | 「調査して」「リサーチして」 |
| troubleshooting | 「デバッグ」「バグ調査」 |
| critical-think | 「批判的に見て」「自己レビュー」 |
| discovery | 「壁打ちしたい」「問題を整理したい」 |
| dialogue | 「対話したい」「議論したい」「意見を聞きたい」「反論して」 |
| problem-domain-modeling | 「課題領域モデリング」「語彙採取」「認知の可視化」「モデリングセッション」 |
| retrospective | 「学びを保存」「ドメイン知識更新」「振り返り」「retrospective」 |
| doc-architect | 「ドキュメント設計」「docs作成」 |
| todoist | 「Todoist」「タスク追加」「todo確認」 |
| todoist-refine | 「タスクリファイン」「タスク整理」「タスク明確化」 |
| setup-linter-hooks | 「linter hook作成」「formatter hook設定」 |
| drawio | 「drawio」「.drawio」「図を作って」 |
| claude-code-release | 「Claude Code リリースノート」「最新バージョン」「アップデート内容」 |
| claude-config | 「設定を最適化」「config」「Claude設定チェック」「ベストプラクティス反映」 |
| harness-tuning | 「Steering Loop が浅い」「診断が機能しない」「Agent出力が不十分」「harness-tuning」 |
| wt | 「/wt」「wt init」「wt switch」「worktree作成」「ワークツリー作成」 |
| security-audit | 「セキュリティ監査」「セキュリティチェック」「パッケージセキュリティ」「サプライチェーン」 |
### 知識系スキル（user-invocable: false）

| スキル | トリガーキーワード | 内容 |
|--------|------------------|------|
| skill-design-patterns | 「スキルを作って」「SKILL.md編集」「スキル品質」 | スキル設計品質パターン参照ガイド |
| ghq | 「ghq」「リポジトリ管理」 | リポジトリ管理ツールガイド |
| glab | 「glab」「GitLab」「MR」「マージリクエスト」「GitLab CI」 | GitLab CLI操作ガイド |
| frontend-design | 「UI作成」「フロントエンド実装」 | UI/フロントエンド実装ガイド |
| playwright-cli | 「Playwright」「E2Eテスト」「ブラウザ操作」「UI確認」 | Playwright CLI + ブラウザ操作リファレンス |
| marimo | 「marimo」「リアクティブノートブック」 | リアクティブPython操作ガイド |
| slidev | 「Slidev」「スライド作成」 | Slidevスライド内容生成ガイド |
| log-designing | 「ログ設計」「構造化ログ」 | ログ設計7原則・CLIパターンガイド |
| dashboard-design | 「ダッシュボード設計」「メトリクス選定」「ダッシュボード診断」「KPI設計」「vanity metric」 | ダッシュボード設計判断基準+診断フレームワーク |
| contextual-commits | 「コミットメッセージ」「action lines」「commit body」「コミットして」 | コミットbodyのaction lines規約ガイド |
| serena | 「Serena」「シンボル検索」 | セマンティックコード検索MCPガイド |
| mermaid-syntax | 「Mermaid」「シーケンス図」「フローチャート」「状態遷移図」「ER図」 | Mermaid構文安全ルール+図種選択ガイド |
| ghostty | 「Ghostty」「ghostty設定」「ターミナル設定」「keybind」「テーマ変更」 | Ghostty設定変更・トラブルシューティング支援（プロジェクトスキル: `.claude/skills/`） |
| zsh | 「zsh設定」「エイリアス追加」「abbr」「キーバインド」「Sheldon」 | Zsh設定・アーキテクチャ・Sheldon・zsh-abbrガイド（プロジェクトスキル: `.claude/skills/`） |
| python-single-file-script | 「PEP 723」「uv run」「Pythonスクリプト」「single-file script」「インラインメタデータ」 | PEP 723 + uv によるPythonスクリプト設計規約ガイド |
| python-data-preprocessing | 「データ前処理」「pandas」「Polars」「scikit-learn Pipeline」「データリーケージ」 | ツール選択・パイプライン設計・アンチパターン回避の判断基準ガイド |
| mise | 「mise」「mise設定」「mise.toml」「ランタイムバージョン」「envrc移行」 | miseランタイムバージョン管理・環境変数管理・direnv移行ガイド |
| gogcli | 「gogcli」「gog」「Gmail CLI」「Google Calendar CLI」「Google Workspace」 | Google Workspace CLI操作・認証・サービス別コマンドリファレンス |
| domain-modeling | 「ドメインモデリング」「DDD」「Bounded Context」「Event Storming」「ドメイン設計」 | ドメインモデリング手法選択・記述法・設計原則ガイド |
