# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## リポジトリの概要

macOSとLinuxシステム全体で開発環境の設定を管理するための個人用dotfilesリポジトリ。シンボリックリンクを使用したgitベースのアプローチで、シェル設定、エディタ設定、開発ツールを管理。

## コマンド

```bash
# シンボリックリンクの作成・再作成
./lib/dotfilesLink.sh

# Homebrewパッケージの更新
brew bundle --file=~/.dotfiles/.Brewfile

# 新規インストール
curl -L raw.github.com/swat9013/dotfiles/master/install.sh | sh
```

## アーキテクチャ

### シンボリックリンク方式

`lib/dotfilesLink.sh` が `.` で始まるファイルを `~/.dotfiles` → `~` へシンボリックリンク作成。

- 除外：`.git`, `.gitignore`, `.gitmodule`
- 特別処理：`sheldon/` → `~/.config/sheldon`

### Zsh設定の読み込み順序（.zshrc）

1. 重複パス削除設定（typeset -U）
2. 自動更新チェック（lib/auto_update.sh）- 24時間経過で自動pull
3. Sheldon プラグインマネージャー初期化
4. モジュール化された設定（.zsh/*.zsh）
5. プラットフォーム固有の PATH

### プラグイン管理

**Sheldon**を使用（Oh-My-Zshではない）：
- 設定：`sheldon/plugins.toml`
- 主要プラグイン：zsh-defer、zsh-syntax-highlighting、zsh-completions、pure prompt

## プラットフォーム検出パターン

```bash
if [ "$(uname)" = 'Darwin' ]; then
    # macOS
elif [ "$(expr substr $(uname -s) 1 5)" = 'Linux' ]; then
    # Linux
fi
```

## シークレット管理

- `.gitconfig.local` はgitignore（`.gitconfig.local.sample` からコピーして使用）
- ユーザー固有のgit設定（名前、メール）を含む

## 注目すべきカスタマイズ

- **rm → rmtrash**: 誤削除防止
- **peco**: `C-r` で履歴検索、ディレクトリナビゲーション
- **Gitエイリアス**: `cofeature <name>`, `cofix <name>`, `delete-merged-branch`
- **Docker**: `.zsh/aliases.zsh` に20以上のエイリアス
