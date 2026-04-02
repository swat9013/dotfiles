---
name: mise
user-invocable: false
description: |-
  miseによるランタイムバージョン管理・環境変数管理の設定・運用・direnv移行を支援する知識ベーススキル。
  Use when「mise」「mise設定」「mise.toml」「ランタイムバージョン」「envrc移行」。
---

# mise 知識ベース

## バージョン情報

現在のバージョン: `!`mise version 2>/dev/null | awk '{print $1}' || echo "(未インストール)"``

記録バージョン: `2026.3.17`

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

---

## このdotfilesでの役割

| 機能 | 状態 |
|------|------|
| ランタイムバージョン管理（Node/Python/Ruby等） | 稼働中 |
| 環境変数管理（direnv代替） | 稼働中（direnv削除済み） |
| タスクランナー | 未使用 |

**経緯**: asdf → mise移行済み。direnv → mise `[env]` 移行済み（2026-03-30）。

## Zsh初期化

activate（キャッシュ方式）を使用。sheldon/starshipと同パターン。

- 実装: `.zsh/mise.zsh`（dotfilesプロジェクト内）
- **キャッシュ注意**: miseアップデート後は `rm ~/.cache/mise/activate.zsh` が必要
- shims方式は不採用（~120ms/コマンドのオーバーヘッド）

---

## コマンド早見表

```bash
mise use node@22          # プロジェクトにNode 22を設定（mise.tomlに記録）
mise use -g node@22       # グローバルにNode 22を設定
mise install              # mise.toml/.tool-versionsのツールをインストール
mise ls                   # インストール済みツール一覧
mise outdated             # 更新可能なツール一覧
mise upgrade              # 全ツール更新
mise x -- <cmd>           # mise環境下でコマンド実行（activate未適用環境用）
mise trust                # カレントディレクトリのmise.tomlを信頼
mise settings             # 設定一覧
```

---

## 設定ファイル優先順位

```
プロジェクト mise.toml > プロジェクト .tool-versions > グローバル ~/.tool-versions > グローバル ~/.config/mise/config.toml
```

新規設定は `mise.toml` 推奨（`[env]`、`[tasks]` が使えるため）。

---

## 環境変数管理（direnv代替）

direnvは削除済み。環境変数管理はmiseの `[env]` で行う。

### プロジェクトでの設定方法

```toml
# mise.toml
[env]
DATABASE_URL = "postgres://localhost/mydb"
SECRET_KEY = "dev-only-key"

# .envファイル読み込み
_.file = ".env"

# PATH追加
_.path = ["./bin", "./node_modules/.bin"]
```

### .envrc からの移行パターン

| .envrc の記述 | mise.toml の対応 |
|--------------|-----------------|
| `export FOO=bar` | `[env]` セクションに `FOO = "bar"` |
| `dotenv` / `source_env .env` | `_.file = ".env"` |
| `PATH_add ./bin` | `_.path = ["./bin"]` |
| `layout python` | `[tools]` セクションに `python = "3.x"` + `_.path = [".venv/bin"]` |
| `use node` | `[tools]` セクションに `node = "lts"` |

詳細は [references/env-migration.md](references/env-migration.md) を参照。

---

## Gotchas

| 罠 | 説明 |
|----|------|
| キャッシュの陳腐化 | miseアップデート後、`rm ~/.cache/mise/activate.zsh` を忘れると古いhookが残る |
| GUI起動プロセス | IDEのGUI起動等ではactivateが効かない。`mise x -- command` で明示実行 |
| fuzzy version | `lts`、`latest` 等はmise固有。`.tool-versions` に書くとasdf互換を失う |
| mise trust | 新プロジェクトの `mise.toml` は初回 `mise trust` が必要（セキュリティ） |
| [env]の評価タイミング | `cd` 時に自動評価。`mise.toml` 変更後は `cd .` で再読み込み |

---

## 詳細リファレンス

| ファイル | 内容 |
|---------|------|
| `references/env-migration.md` | .envrc → mise.toml [env] の詳細移行ガイド、高度なパターン、トラブルシューティング |
