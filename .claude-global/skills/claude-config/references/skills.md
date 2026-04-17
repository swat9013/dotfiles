# Skills リファレンス

## TOC
[配置場所と優先度](#配置場所と優先度) / [Skills vs Commands](#skills-vs-commands) / [skills vs agents vs rules](#skills-vs-agents-vs-rules) / [context: fork の agent 値](#context-fork-の-agent-値) / [skills フィールド](#skills-フィールド) / [Skill(name *) 権限制御](#skill-name--権限制御) / [フィードバックループパターン](#フィードバックループパターン) / [スキル分類](#スキル分類設計時の参考) / [On Demand Hooks](#on-demand-hooks) / [データ永続化](#データ永続化) / [スキル間の依存・計測](#スキル間の依存・計測) / [トラブルシューティング](#トラブルシューティング)

---

## 配置場所と優先度

| 場所 | パス | 優先度 |
|------|------|--------|
| Enterprise | 組織管理設定 | 1（最高） |
| Personal | `~/.claude/skills/` | 2 |
| Project | `.claude/skills/` | 3 |
| Plugin | `plugin-name:skill-name` | 4（名前空間分離） |

- 同名スキルは優先度の高い方が使用される
- skills と commands が同名の場合、**skills が優先**
- `.claude/commands/` は後方互換性あり、skills への移行を推奨

---

## Skills vs Commands

| 観点 | Skill | Command |
|------|-------|---------|
| 用途 | 対話・相談・ワークフロー | アクション実行（定型プロンプト） |
| 呼び出し | Claude 自動選択 or `/skill-name` | `/コマンド名`（明示のみ） |
| 構造 | ディレクトリ（複数ファイル可） | 単一ファイル |
| 例 | code-review, debug | commit, deploy |

新規作成は skills を推奨（上位互換）。

---

## skills vs agents vs rules

| 観点 | skills/ | agents/ | rules/ |
|------|---------|---------|--------|
| 用途 | ワークフロー・手順定義、知識・参照情報 | カスタムサブエージェント | パス固有のルール |
| 実行形態 | 対話型・相談型 | コンテキスト分離実行 | 自動適用 |
| トリガー | Claude 自動選択 or `/skill-name` | Task tool 経由 | ディレクトリアクセス時 |
| コンテキスト | 共有（会話継続） | 分離（独立実行） | - |
| 適用場面 | 手順定義、ガイド | 並列実行・試行錯誤・専門役割 | - |

---

## context: fork の agent 値

```yaml
context: fork
agent: Explore   # 探索・調査系
# agent: Plan    # 計画・設計系
# agent: general-purpose  # 汎用
```

- `context: fork` でサブエージェントとして隔離実行
- SKILL.md の内容がサブエージェントへのプロンプトになる
- `agent` フィールドで種別を指定（省略時は general-purpose）
- **制約: fork内からAgent/Task/Skill toolは使用不可**（孫スポーン不可。1段階のみ）
- `model` フィールドで実行モデルを指定可能（省略時は親セッションのモデルを継承）

**fork適用の前提条件チェック**:
1. スキル内でAgent/Task toolを使っていないか → 使っていたら fork 不可
2. 対話型（AskUserQuestion等）でないか → 対話型なら fork 不可
3. 直前の会話コンテキストに依存しないか → 依存するなら fork 不可

動的コンテキスト注入との組み合わせ例:

```yaml
---
name: pr-summary
context: fork
agent: Explore
---
## PR context
- PR diff: !`gh pr diff`
- Changed files: !`gh pr diff --name-only`

Summarize this pull request...
```

---

## skills フィールド

サブエージェント（`.claude/agents/` または `context: fork`）の frontmatter に `skills:` を指定すると、起動時にスキル内容をコンテキストへ注入できる:

```yaml
---
name: api-developer
description: チーム規約に従い API エンドポイントを実装する。
skills:
  - api-conventions
  - error-handling-patterns
---
```

- 指定スキルの**完全な内容**がサブエージェント起動時にコンテキストへ注入
- サブエージェントは親セッションのスキルを**継承しない** — 必要なスキルは明示指定
- 「利用可能にする」ではなく「実際にコンテキストへロード」する点に注意

---

## Skill tool 経由で呼び出せる組み込みコマンド

v2.1.108+ で、`/init` / `/review` / `/security-review` はモデルが Skill tool 経由で発見・呼び出し可能。`/compact` など他の組み込みコマンドはユーザーからの明示的スラッシュ入力のみ。

---

## Skill(name *) 権限制御

`settings.json` の `permissions` で特定スキルへのアクセスを制御:

```json
{
  "permissions": {
    "allow": ["Skill(commit)", "Skill(review-pr *)"],
    "deny": ["Skill(deploy *)"]
  }
}
```

- `Skill(name)` — 特定スキルを許可/拒否
- `Skill(name *)` — ワイルドカードで複数スキルを対象
- `deny` が `allow` より優先（通常の permission ルールと同様）

---

## フィードバックループパターン

品質向上には「実行 → 検証 → 修正 → 繰り返し」パターンが効果的。スキル内に明示的なチェックループを記述しておくと Claude が自律的に品質を保つ。

## スキル分類（設計時の参考）

| 分類 | 目的 | 例 |
|------|------|-----|
| Library & API Reference | 内部ライブラリ・CLIの正しい使い方とGotchas | `billing-lib`, `internal-platform-cli` |
| Product Verification | コード動作の検証。Playwright/tmux等と連携 | `signup-flow-driver`, `checkout-verifier` |
| Data Fetching & Analysis | データ・モニタリングスタックへの接続と分析 | `funnel-query`, `grafana` |
| Business Process & Team Automation | 定型ワークフローの自動化 | `standup-post`, `weekly-recap` |
| Code Scaffolding & Templates | フレームワークボイラープレート生成 | `new-migration`, `create-app` |
| Code Quality & Review | コード品質の強制とレビュー | `adversarial-review`, `code-style` |
| CI/CD & Deployment | ビルド・デプロイ・PR管理 | `babysit-pr`, `deploy-service` |
| Runbooks | 症状→調査→構造化レポートの自動化 | `oncall-runner`, `log-correlator` |
| Infrastructure Operations | メンテナンス・運用手順にガードレール付与 | `dependency-management`, `cost-investigation` |

良いスキルは1分類に収まる。複数にまたがると設計が曖昧になりやすい。

## On Demand Hooks

スキル呼び出し時のみ有効化され、セッション終了まで持続するhooks。frontmatter の `hooks:` フィールドで定義する（構文は settings.json の hooks と同一）。

- 常時有効だと邪魔だが特定作業時には必須、というガードレールに最適
- 例: `/careful` で `rm -rf`, `DROP TABLE`, force-push をブロック
- 例: `/freeze` で特定ディレクトリ外の Edit/Write をブロック

## データ永続化

`${CLAUDE_PLUGIN_DATA}`: プラグインごとの安定ストレージパス。スキルディレクトリ内のデータはアップグレード時に消えるため、永続データ（ログ・設定JSON・SQLite等）はこのパスに保存する。

## スキル間の依存・計測

- **依存**: ネイティブ未対応。SKILL.md内で名前参照すればインストール済みの場合にモデルが自動呼び出し。未インストール時は無視
- **計測**: `PreToolUse` hookでスキル呼び出しをログ記録し、人気度・トリガー率を分析可能

---

## トラブルシューティング

### スキルが発動しない
1. description にユーザーが使う言葉を含めているか確認
2. `when_to_use` フィールドでトリガーフレーズ・例示リクエストを追加（v2.1.105+。文字上限は下記「スキルが読み込まれない」参照）
3. `What skills are available?` でスキル一覧を確認
4. 直接 `/skill-name` で呼び出してテスト

### スキルが頻繁に誤発動する
1. description をより具体的に
2. `disable-model-invocation: true` で手動呼び出しのみに制限

### スキルが読み込まれない
- skills description バジェット: コンテキストウィンドウの **1%**（fallback: 8,000文字）。各エントリは **1,536文字**（`description` + `when_to_use` 合算）でトランケート
- `/context` で除外されたスキルを確認
- `SLASH_COMMAND_TOOL_CHAR_BUDGET` 環境変数で上限を調整可能
- **ライブ変更検出**（v2.1.105+）: `~/.claude/skills/` / プロジェクト `.claude/skills/` / `--add-dir` 内の `.claude/skills/` はセッション中の追加・編集・削除が即時反映。例外: セッション開始時に存在しなかったトップレベル skills ディレクトリ自体の新規作成は再起動が必要

### スキルのコンテキストライフサイクル

- 呼び出し後、セッション全体でコンテキストに残存
- auto-compaction 時は最近呼び出したスキルから優先的に再アタッチ（5,000トークン/スキル、合計25,000トークンのバジェット）
