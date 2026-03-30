---
name: worktrunk
user-invocable: false
description: |-
  dotfilesのworktrunk統合（シェル初期化・config管理方針・.wtp.yml移行）を提供する知識ベーススキル。
  Use when「worktrunk設定」「wt設定」「worktree管理」「worktrunk dotfiles」「wtp移行」。
---

# Worktrunk 知識ベース

マーケットプレイスプラグイン `worktrunk@worktrunk` がコマンド・設定・フック仕様をカバー。このスキルはdotfiles固有の統合情報を補完する。

## バージョン情報

現在: !`wt --version 2>/dev/null || echo "(未インストール)"`
記録: 0.33.0

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

## このdotfilesの現行worktrunk統合サマリ

| 項目 | 値 | ファイル |
|------|-----|---------|
| Brewfile | `brew "worktrunk"` | `.Brewfile` |
| shell init | `eval "$(command wt config shell init zsh)"` | `.zshrc:51` |
| user config | 未作成（正常。デフォルト動作で十分） | `~/.config/worktrunk/config.toml` |
| プラグイン | `worktrunk@worktrunk` v0.33.0 有効化 | `.claude-global/settings.json` |

## shell init 設計判断

`.zshrc:51` に `command -v wt` ガード付きで配置:

```zsh
if command -v wt >/dev/null 2>&1; then eval "$(command wt config shell init zsh)"; fi
```

**配置位置の理由**: sheldon（プラグイン）・starship（プロンプト）の初期化より後。worktrunkのshell関数は `cd` をラップするため、他のツールの初期化完了後に評価する必要がある。

## user config の dotfiles 管理方針

```
config.toml をdotfilesで管理するか？
├─ LLM設定（APIキー、モデル選択）→ マシン固有。管理しない
├─ worktree-path テンプレート → デフォルトで十分。カスタマイズ不要
└─ merge/list デフォルト → 個人の好み。管理しない

→ dotfilesLink.sh への追加は不要
```

config作成が必要になった場合: `wt config create` で生成。dotfiles管理に含めない。

## .wtp.yml 移行

プロジェクトに `.wtp.yml` が残っている場合、`.config/wt.toml` に変換する。

**基本方針**:
- ディレクトリ配置・命名規則は wt デフォルトを維持（.wtp.yml の `base_dir` は変換しない）
- `hooks.post_create` → wt の `pre-start`（ブロッキング）または `post-start`（バックグラウンド）に変換

詳細な変換マッピングと手順: [references/wtp-migration.md](references/wtp-migration.md)

## Gotchas

| 罠 | 説明 |
|----|------|
| shell init 配置順 | sheldon/starship より後に配置が必要。`.zshrc` 内での順序を変更しない |
| config 未作成 | `wt config show` はエラーにならずデフォルト値を表示。config.toml 不在は正常 |
| プラグイン更新乖離 | `brew upgrade worktrunk` でCLI更新してもプラグインは別途更新が必要 |
| .wtp.yml の base_dir | wt の worktree-path テンプレートに変換不要。wt デフォルトを維持 |
| copy-ignored の範囲 | `wt step copy-ignored` は gitignored files のみ対象。`.claude` のような git 管理ファイルは個別 `cp` コマンドが必要 |

## 詳細リファレンス

| ファイル | 内容 |
|---------|------|
| `references/wtp-migration.md` | .wtp.yml → .config/wt.toml 変換マッピング・手順・実例 |
