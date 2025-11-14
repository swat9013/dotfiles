# CLAUDE.md

このファイルは、Claude Code (claude.ai/code) がこのリポジトリで作業する際のガイダンスを提供します。

## リポジトリの概要

macOSとLinuxシステム全体で開発環境の設定を管理するための個人用dotfilesリポジトリです。シンボリックリンクを使用したgitベースのアプローチで、シェル設定、エディタ設定、開発ツールを管理します。

## インストールとセットアップ

### 新規インストール

```bash
cd ~
curl -L raw.github.com/swat9013/dotfiles/master/install.sh | sh
```

このスクリプトは以下を実行します：

1. リポジトリを `~/.dotfiles` にクローン
2. `lib/dotfilesLink.sh` を実行してすべてのdotfilesのシンボリックリンクを作成
3. macOSの場合：`.Brewfile` から `brew bundle` でパッケージをインストール
4. `.gitconfig.local` が存在しない場合、`.gitconfig.local.sample` から作成

### 手動での再リンク

変更後にシンボリックリンクを再作成する必要がある場合：

```bash
cd ~/.dotfiles
./lib/dotfilesLink.sh
```

### パッケージの更新

```bash
brew bundle --file=~/.dotfiles/.Brewfile
```

## アーキテクチャ

### シンボリックリンクベースの設定

- `lib/dotfilesLink.sh` スクリプトが `.` で始まるすべてのファイルに対して `~/.dotfiles` から `~` へのシンボリックリンクを作成
- 除外対象：`.git`, `.gitignore`, `.gitmodule`
- 特別な処理：`.gitconfig.local` はsampleから作成、`sheldon/` は `~/.config/sheldon` にシンボリックリンク

### 自動更新メカニズム

- `lib/auto_update.sh` がシェル起動時に実行される（`.zshrc` でsource）
- dotfilesが古くなっているかチェック（閾値：24時間）
- 遅れている場合は自動的に origin/master から更新を取得

### モジュール化されたZsh設定

設定は `.zsh/` 内の専用モジュールに分割されています：

- `aliases.zsh` - git、docker、rails、ruby、python、エディタのコマンドエイリアス（145行）
- `keybinds.zsh` - 履歴・ディレクトリ検索のためのpecoインテグレーションを含むキーバインド
- `opt.zsh` - 履歴オプションとシェルの動作
- `completion.zsh` - 補完システムのセットアップ

`.zshrc` での読み込み順序：

1. 重複パス削除設定（typeset -U）
2. 自動更新用設定（DISABLE_UPDATE_PROMPT、UPDATE_ZSH_DAYS）
3. 自動更新チェック（lib/auto_update.sh）
4. Sheldon プラグインマネージャーの初期化
5. モジュール化された zsh 設定（.zsh/*.zsh）
6. プラットフォーム固有の PATH 追加（macOS、Go、asdf、ローカルbin）
7. Rancher Desktop PATH

### プラグイン管理

zshプラグイン管理には **Sheldon**（Oh-My-Zshではない）を使用：

- 設定：`sheldon/plugins.toml`
- ロックファイル：`sheldon/plugins.lock`（gitignored）
- 主要プラグイン：zsh-defer、zsh-syntax-highlighting、zsh-completions、pure prompt

## 主要な設定ファイル

### シェル環境

- `.zshrc` - メインのzshエントリーポイント、モジュール化された設定を読み込む
- `.zshenv` - 環境変数（ロケール、エディタ、履歴設定）
- `.zsh/*.zsh` - モジュール化された設定ファイル

### Git設定

- `.gitconfig` - ワークフロー用の50以上のエイリアスを含むメインgit設定
- `.gitconfig.local.sample` - ユーザー認証情報のテンプレート（`.gitconfig.local` にコピー）
- `.gitignore_global` - グローバル無視パターン

注目すべきgitエイリアス：

- ブランチ作成：`cofeature <name>`、`cofix <name>`
- クリーンアップ：`delete-merged-branch`
- 高度な機能：`sed`（ファイル内のsed置換）、`subtree` 操作

### パッケージ管理

- `.Brewfile` - macOSセットアップ用の60個以上のHomebrewパッケージ/cask（brew 48個、cask 13個）
- `.default-gems` - rbenv/asdfでインストールするRuby gem
- `.default-python-packages` - pyenv/asdf用のPythonパッケージ

### エディタ設定

- `.emacs.d/init.el` - メインのEmacs設定
- `.emacs.d/init.d/` - モジュール化されたEmacs設定（common、mac、package）
- `.editorconfig` - コードスタイル（UTF-8、LF、2スペースインデント）

### ターミナル/マルチプレクサ

- `.tmux.conf` - カスタムプレフィックス（C-z）、viキーバインドを持つTmux
- `.tmuxinator.yml` - Tmuxセッションテンプレート
- `com.googlecode.iterm2.plist` - iTerm2の設定

## プラットフォーム検出パターン

dotfiles全体で、プラットフォーム固有のコードは以下を使用：
```bash
if [ "$(uname)" = 'Darwin' ]; then
    # macOS固有のコード
elif [ "$(expr substr $(uname -s) 1 5)" = 'Linux' ]; then
    # Linux固有のコード
elif [ "$(expr substr $(uname -s) 1 10)" = 'MINGW32_NT' ]; then
    # Cygwin固有のコード
fi
```

## カスタムスクリプト

### scripts/vibe-kanban.sh

vibe-kanban Kanbanボードサーバーのラッパー：

```bash
./scripts/vibe-kanban.sh {start|stop|restart|status}
```

- nohupでデーモンとして実行
- `work/logs/vibe_kanban.log` にログ出力

### lib/ide.sh

tmux IDEセッションを起動

### lib/ssh_host_color.sh

視覚的な区別のため、ホスト名でSSH接続に色を付ける

### lib/auto_update.sh

dotfilesの自動更新スクリプト（シェル起動時に実行）

## 設定済み開発ツール

**言語/フレームワーク：**

- Ruby/Rails（Docker、Rubocopエイリアス付き）
- Python（pip、poetry、uv）
- Go（GOPATHハンドリング）
- JavaScript/Node（yarn、npm）
- Java（openjdk経由）

**クラウド/DevOps：**

- AWS CLI と CDK
- Docker/docker-compose（aliases.zshに20以上のエイリアス）
- Rancher Desktop インテグレーション
- Kubernetes（deck）

## 注目すべきカスタマイズ

1. **pecoインテグレーション** - コマンド履歴（`C-r`）とディレクトリナビゲーションのインタラクティブなファジー検索
2. **ゴミ箱保護** - 誤削除を防ぐため `rm` を `rmtrash` にエイリアス
3. **カラー差分** - 統一形式でcolordiffを使用
4. **Emacsデーモン** - コマンド：`e`（emacsclient）、`emacs-kill`（デーモン終了）
5. **Gitワークフローヘルパー** - Feature/Fixブランチの作成、マージ済みブランチのクリーンアップ
6. **Dockerショートカット** - docker/docker-compose操作用の豊富なエイリアス

## シークレット管理

- `.gitconfig.local` はgitignoreされ、`.gitconfig.local.sample` から作成される
- ユーザー固有のgit設定（名前、メール、GitHubトークンなど）を含む
- 常にsampleファイルをテンプレートとして使用し、`.gitconfig.local` は決してコミットしない

## メンテナンス

リポジトリは24時間以上経過している場合、シェル起動時に自動更新されます。手動で更新する場合：

```bash
cd ~/.dotfiles
git pull origin master
```

更新を取得後、シンボリックリンクはリンクスクリプトによって自動的に更新されます。
