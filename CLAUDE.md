# CLAUDE.md

## リポジトリ概要

macOS/Linux開発環境のdotfiles。シンボリックリンク方式でgit管理。

## 主要コマンド

```bash
./lib/dotfilesLink.sh       # シンボリックリンク作成・再作成
brew bundle --global        # Homebrewパッケージ更新
```

## アーキテクチャ概要

- `.`で始まるファイル → `~`へシンボリックリンク
- `sheldon/`, `ghostty/`, `starship/`, `zsh-abbr/` → `~/.config/`配下
- `zed/` → `~/.config/zed/`配下
- `colima/colima.yaml` → `~/.colima/default/`配下
- `.claude-global/` → `~/.claude/`配下

## コーディング原則

- シンプルさ優先（KISS）
- 既存パターンに従う（新規ツールは`.zsh/[tool].zsh`）
- 詳細は各`rules/`ファイルを参照
- 同じ指示を CLAUDE.md と rules/ に重複記載しない

## Harness Architecture 原則

- **Guide First, Sense Second** — 事前方向づけを優先する。事後検出は逸脱確認に限定し、予防的誘導を先行させる。
- **Computational First** — 計測可能な基準はスクリプト化する。判断が必要なものだけ推論に委ねる。
- **ライフサイクル配置** — 制御をタスク開始時・実行中・実行後・セッション後・定期の各フェーズに分散させる。
- **自己メンテナンス** — Harness 自体の劣化を検出・修正する仕組みを組み込む。陳腐化を放置しない。

詳細は `rules/harness-architecture.md` を参照

## Gotchas

- **`.git/info/exclude` の `/.claude` 罠**: `.claude/skills/` 等を git 追跡したい場合は `.git/info/exclude` に `/.claude` が混入していないか確認。`.gitignore` の個別除外よりも `exclude` が優先されてディレクトリ全体をブロックする。修正: `exclude` から `/.claude` 行を削除し `.gitignore` で細粒度管理。

