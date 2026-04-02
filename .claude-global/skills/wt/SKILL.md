---
name: wt
description: |-
  プロジェクト構造を分析してworktrunk初期設定を生成し、タスクからブランチ名生成+worktree作成する。
  Use when「/wt」「wt init」「wt switch」「worktree作成」「ワークツリー作成」。
user-invocable: true
disable-model-invocation: true
argument-hint: "init | switch [タスク説明]"
---

# wt

サブコマンド: `$ARGUMENTS[0]`

## init

プロジェクト構造を分析し、適切なhooks付きの `.config/wt.toml` を生成する。

### 手順

1. `.config/wt.toml` が既に存在する場合 → エラー報告して終了
2. プロジェクトルートのファイルを確認し、技術スタックを検出:

| ファイル | スタック | pre-start | pre-commit | pre-merge |
|---------|---------|-----------|------------|-----------|
| `package.json` | Node.js | npm ci | npm run lint | npm test |
| `pnpm-lock.yaml` | pnpm | pnpm install | pnpm lint | pnpm test |
| `yarn.lock` | Yarn | yarn install | yarn lint | yarn test |
| `bun.lockb` | Bun | bun install | bun lint | bun test |
| `Makefile` | Make | make install | make lint | make test |
| `pyproject.toml` | Python | pip install -e . | ruff check | pytest |
| `Gemfile` | Ruby | bundle install | bundle exec rubocop | bundle exec rspec |
| `go.mod` | Go | go mod download | golangci-lint run | go test ./... |
| `Cargo.toml` | Rust | cargo build | cargo clippy | cargo test |
| `mix.exs` | Elixir | mix deps.get | mix credo | mix test |

上記は目安。実際のscriptsセクション（package.jsonの`scripts`等）を読み、プロジェクトに合ったコマンドを判断する。

3. 検出結果から `.config/wt.toml` を生成。構造:

```toml
# Worktrunk project hooks
# Docs: https://worktrunk.dev/hook/

[pre-start]
install = "<dependency install command>"

[post-start]
copy-ignored = "wt step copy-ignored"

[pre-commit]
lint = "<lint command>"

[pre-merge]
test = "<test command>"
```

- 該当しないセクションはコメントアウトして残す
- `post-start.copy-ignored` は常に含める（gitignored filesのコピー）

4. 生成内容をコードブロックで提示 → AskUserQuestion で確認
5. 承認後、`mkdir -p .config` → Writeで `.config/wt.toml` を書き出し

## switch

タスク説明からブランチ名を生成し、worktreeを作成する。

### 手順

1. `$ARGUMENTS` が `switch` のみ（タスク説明なし）の場合 → AskUserQuestion でタスク内容を聞く
2. 既存ブランチの命名パターンを分析:

```bash
git branch -a --format='%(refname:short)'
```

パターン例:
- `feat/xxx`, `fix/xxx` → Conventional型
- `feature/xxx`, `bugfix/xxx` → GitFlow型
- `JIRA-123-xxx` → チケットID型
- パターンなし → デフォルト `type/slug`

3. タスク内容 + 検出パターンからブランチ名を生成:
   - 検出パターンに従う
   - slug は英語 kebab-case、3〜5語
   - Conventional型のtype: feat, fix, refactor, docs, chore, test

4. AskUserQuestion で提示（ユーザーが編集可能）
5. 承認後に実行:

```bash
wt switch -c <ブランチ名>
```

6. 実行結果を表示して完了
