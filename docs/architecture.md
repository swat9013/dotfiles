# Dotfiles アーキテクチャ

macOSとLinuxで開発環境の設定を管理するための個人用dotfilesリポジトリ。

## ディレクトリ構造

```
~/.dotfiles/
├── lib/                          # ユーティリティスクリプト
│   ├── dotfilesLink.sh           # シンボリックリンク管理
│   ├── auto_update.sh            # 自動更新チェック (24時間間隔)
│   ├── claude-statusline.sh      # Claude Code ステータスライン
│   ├── claude-file-suggestion.sh # Claude Code ファイル提案
│   ├── macos.sh                  # macOS固有設定
│   ├── ide.sh                    # tmux IDE レイアウト
│   ├── vibe-kanban.sh            # Kanban サーバー管理
│   ├── fortivpn.sh               # VPN接続ラッパー
│   └── ssh_host_color.sh         # SSH ホスト色分け
├── .zsh/                         # Zsh設定モジュール
│   ├── aliases.zsh               # エイリアス定義 (150+)
│   ├── keybinds.zsh              # キーバインド (peco統合)
│   ├── opt.zsh                   # シェルオプション
│   └── completion.zsh            # 補完設定
├── .emacs.d/                     # Emacs設定 (straight.el)
│   ├── init.el                   # メインエントリポイント
│   ├── early-init.el             # 起動前設定
│   └── lisp/                     # モジュール化された設定
├── sheldon/                      # Zsh プラグインマネージャー
│   └── plugins.toml              # プラグイン定義
├── ghostty/                      # Ghostty ターミナル設定
├── .claude-global/               # Claude Code グローバル設定
├── docs/                         # ドキュメント
├── .gitconfig                    # Git設定 (60+ エイリアス)
├── .zshenv                       # Zsh環境変数
├── .zshrc                        # Zshメイン設定
├── .tmux.conf                    # Tmux設定
├── .Brewfile                     # Homebrewパッケージ定義
├── install.sh                    # インストールスクリプト
└── CLAUDE.md                     # Claude Code用指示ファイル
```

## シンボリックリンク方式

`lib/dotfilesLink.sh` が設定ファイルの同期を管理。

```
~/.dotfiles/.zshrc      →  ~/.zshrc
~/.dotfiles/.gitconfig  →  ~/.gitconfig
~/.dotfiles/sheldon/    →  ~/.config/sheldon/
~/.dotfiles/ghostty/    →  ~/.config/ghostty/
```

### 除外対象
- `.git`, `.gitignore`, `.gitmodule`
- `.gitconfig.local.sample` (`.gitconfig.local` にコピーして使用)
- `.claude`, `.claude-global`

### 特別処理
- `sheldon/` → `~/.config/sheldon`
- `ghostty/` → `~/.config/ghostty`
- `.claude-global/settings.json` → `~/.claude/settings.json`

## Zsh設定の読み込みフロー

```
1. .zshenv (ログイン時・非対話シェル)
   ├── ロケール設定 (ja_JP.UTF-8)
   ├── エディタ環境変数 (code/emacs/vim)
   ├── History設定
   ├── プラットフォーム検出
   ├── Homebrew初期化 (M1 Mac対応)
   └── ASDF PATH設定

2. .zshrc (対話シェルのみ)
   ├── パス重複排除 (typeset -U)
   ├── 自動更新チェック (lib/auto_update.sh)
   ├── Sheldonプラグイン初期化
   ├── .zshモジュール読み込み
   │   ├── aliases.zsh
   │   ├── keybinds.zsh
   │   ├── opt.zsh
   │   └── completion.zsh
   └── 追加PATH設定
```

## プラグイン管理 (Sheldon)

Oh-My-Zshではなく **Sheldon** を採用（軽量・高速）。

| プラグイン | 用途 |
|-----------|------|
| zsh-defer | 遅延読み込み、起動時間最適化 |
| zsh-syntax-highlighting | コマンド構文ハイライト |
| zsh-completions | 高度な補完機能 |
| ohmyzsh-lib | Oh-My-Zshのlib部分のみ使用 |
| pure | モダンプロンプト (非同期git情報) |

## lib/ユーティリティスクリプト

| スクリプト | 機能 |
|-----------|------|
| dotfilesLink.sh | シンボリックリンク作成・管理 |
| auto_update.sh | 24時間経過で自動git pull |
| claude-statusline.sh | ステータスライン表示 (Git/コンテキスト使用率) |
| claude-file-suggestion.sh | ファイル提案 (ripgrep使用) |
| macos.sh | macOS input source切り替え無効化 |
| ide.sh | tmux IDE用パネル配置 |
| vibe-kanban.sh | npx vibe-kanban の起動・停止管理 |
| fortivpn.sh | OpenFortiVPN ラッパー |

## プラットフォーム対応

```bash
if [ "$(uname)" = 'Darwin' ]; then
    # macOS固有設定
elif [ "$(expr substr $(uname -s) 1 5)" = 'Linux' ]; then
    # Linux固有設定
fi
```

### macOS固有
- Homebrew (M1/Intel対応)
- reattach-to-user-namespace (tmux clipboard)
- lib/macos.sh (input source設定)

### Linux固有
- Emacsenv PATH設定

## Claude Code統合

### パーミッション設定 (.claude-global/settings.json)

```json
{
  "permissions": {
    "allow": ["Bash(npm run:*)", "Bash(git diff:*)", "Bash(git status)", ...],
    "deny": ["Read(./.env)", "Read(**/*.key)", "Bash(wget:*)"],
    "ask": ["Bash(curl:*)", "Bash(git push:*)", "Bash(rm:*)"]
  }
}
```

### ステータスライン (lib/claude-statusline.sh)

表示形式: `directory git:branch* | Ctx:XX% | "session title..."`

- Git情報取得
- コンテキスト使用率計算 (色分け: Green/Yellow/Red)
- セッションタイトル表示

### ファイル提案 (lib/claude-file-suggestion.sh)

ripgrepで全ファイル検索 (.gitignore無視、隠しファイル含む、lockファイル除外)

## Zshエイリアス概要

| カテゴリ | 例 |
|---------|-----|
| 標準コマンド | `grep='grep --color=auto'` |
| エディタ | `c='code'`, `e()` (emacsclient) |
| Git | `g='git'`, `L` (peco), `LA`, `R` |
| Tmux | `tm`, `ide` |
| Docker | `dcew`, `dcrw`, `dclog` (20+) |
| Rails | `con`, `db_migrate` |
| 削除保護 | `rm='rmtrash'` |
| VPN/Kanban | `fvpn`, `vk` |

## キーバインド (.zsh/keybinds.zsh)

| キー | 機能 |
|------|------|
| Ctrl+r | peco履歴検索 |
| Ctrl+s | pecoディレクトリナビゲーション |
| Ctrl+^ | 一つ上のディレクトリへ |
| Ctrl+m | 入力なし→ls+git status |

## Emacs設定 (straight.el + use-package)

### モジュール構成

| モジュール | 内容 |
|-----------|------|
| init-core.el | コア設定、UI改善 |
| init-completion.el | 補完 (Cape, Company, Consult) |
| init-corfu.el | Corfu UI補完 |
| init-lsp.el | LSP統合 |
| init-treesit.el | Tree-sitter統合 |
| init-lang-ruby.el | Ruby/Rails対応 |
| init-lang-ts.el | TypeScript/JavaScript |
| init-lang-web.el | Web開発 |
| init-git.el | Magit, forge |
| init-project.el | Projectile |

## Git設定 (.gitconfig)

### 主要エイリアス

| エイリアス | 機能 |
|-----------|------|
| cofeature \<name\> | feature/\<name\>ブランチ作成 |
| cofix \<name\> | fix/\<name\>ブランチ作成 |
| delete-merged-branch | マージ済みブランチ削除 |
| aicommit | Claude でConventional Commits生成 |

## Tmux設定 (.tmux.conf)

- prefix: `C-z`
- ペイン分割: `|` (垂直), `-` (水平)
- Vi キーバインド (コピーモード)
- Powerline統合

## Ghostty設定 (ghostty/config)

- テーマ: Catppuccin (ライト/ダーク自動切り替え)
- Emacs風スプリット移動 (Ctrl+Shift+b/f/p/n)

## Brewfile (.Brewfile)

45+パッケージを管理:

- CLI: asdf, emacs, git, jq, peco, ripgrep, sheldon, tmux, zsh
- GUI: dash, dbeaver-community, devtoys, obs
- Fonts: font-*

## セキュリティ

### gitignore対象
- `.emacs.d/straight/repos/`, `build/`, `var/`, `etc/`
- `.sheldon/repos`
- `logs/`

### シークレット管理
- `.gitconfig.local.sample` → `.gitconfig.local` にコピー (gitignore)
- Claude Code: `.env`, `*.key` のReadをdeny

## 自動更新 (lib/auto_update.sh)

24時間経過で自動 `git pull`:

1. `~/.last_update` タイムスタンプをチェック
2. HEAD vs origin/master 比較
3. 差分あれば自動pull
4. タイムスタンプ更新
