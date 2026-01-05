# Zsh設定ファイル管理のベストプラクティス

## 1. 概要

本ドキュメントでは、Zshの設定ファイル構造、管理方法、およびベストプラクティスについてまとめる。

## 2. Zsh起動ファイルの読み込み順序

### 2.1 ファイル一覧と読み込み順序

```
.zshenv → .zprofile → .zshrc → .zlogin → .zlogout
```

| ファイル | インタラクティブログイン | インタラクティブ非ログイン | スクリプト |
|----------|--------------------------|----------------------------|------------|
| /etc/zshenv | A | A | A |
| ~/.zshenv | B | B | B |
| /etc/zprofile | C | - | - |
| ~/.zprofile | D | - | - |
| /etc/zshrc | E | C | - |
| ~/.zshrc | F | D | - |
| /etc/zlogin | G | - | - |
| ~/.zlogin | H | - | - |
| ~/.zlogout | I (終了時) | - | - |

### 2.2 各ファイルの役割

| ファイル | 用途 | 配置すべき内容 |
|----------|------|----------------|
| `.zshenv` | 全シェルで読み込み | 環境変数（EDITOR, LANG等）、ZDOTDIR設定 |
| `.zprofile` | ログインシェル時（.zshrc前） | PATH設定（macOSでは`path_helper`の後に読まれる） |
| `.zshrc` | インタラクティブシェル時 | エイリアス、関数、プロンプト、キーバインド、補完設定 |
| `.zlogin` | ログインシェル時（.zshrc後） | ログイン時のみ実行したいコマンド |
| `.zlogout` | ログアウト時 | クリーンアップ処理 |

**重要**: `.zshenv`は出力を生成するコマンドやTTYを前提とするコマンドを含めるべきではない。

## 3. 推奨ディレクトリ構造

### 3.1 基本構造（現在のdotfiles）

```
~/.dotfiles/
├── .zshenv              # 環境変数、補完の初期化
├── .zshrc               # メイン設定（プラグイン読み込み、モジュール読み込み）
├── .zsh/                # モジュール化された設定
│   ├── aliases.zsh      # エイリアス定義
│   ├── keybinds.zsh     # キーバインド設定
│   ├── opt.zsh          # シェルオプション（履歴設定等）
│   └── completion.zsh   # 補完設定
├── sheldon/             # プラグインマネージャー設定
│   └── plugins.toml
└── lib/                 # シェルスクリプト・ユーティリティ
    └── auto_update.sh
```

### 3.2 XDG準拠構造（代替案）

```
~/.zshenv                     # ZDOTDIR設定のみ（ブートストラップ）
~/.config/zsh/                # ZDOTDIR
├── .zshrc
├── .zprofile
├── conf.d/                   # モジュール化された設定
│   ├── 00-options.zsh
│   ├── 10-aliases.zsh
│   └── 20-keybinds.zsh
└── functions/                # 自動読み込み関数
~/.local/share/zsh/           # データ（履歴等）
~/.cache/zsh/                 # キャッシュ（補完キャッシュ等）
```

XDG準拠にする場合の`~/.zshenv`:
```zsh
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
```

## 4. モジュール化のベストプラクティス

### 4.1 ファイル分割の指針

| カテゴリ | ファイル名例 | 内容 |
|----------|--------------|------|
| オプション | `opt.zsh` | `setopt`による動作設定 |
| 履歴 | `history.zsh` | 履歴関連の設定 |
| エイリアス | `aliases.zsh` | コマンドエイリアス |
| キーバインド | `keybinds.zsh` | `bindkey`設定 |
| 補完 | `completion.zsh` | `compinit`、`zstyle`設定 |
| 関数 | `functions.zsh` | カスタム関数定義 |
| プラットフォーム | `darwin.zsh`, `linux.zsh` | OS固有設定 |

### 4.2 命名規則

- 拡張子: `.zsh`を使用
- ファイル名: 小文字とアンダースコア（例: `utility_functions.zsh`）
- 読み込み順序を制御する場合: 数字プレフィックス（例: `00-`, `10-`）

### 4.3 モジュール読み込みパターン

```zsh
# 現在のdotfilesで使用しているパターン
for conf in $HOME/.dotfiles/.zsh/*.zsh; do
    source ${conf}
done

# 順序制御が必要な場合
for conf in $ZDOTDIR/conf.d/*.zsh(N); do
    source "$conf"
done
```

## 5. プラグイン管理

### 5.1 プラグインマネージャー比較

| マネージャー | 特徴 | パフォーマンス |
|--------------|------|----------------|
| **Sheldon** | Rust製、TOML設定、遅延読み込み対応 | 高速 |
| Zinit | 多機能、複雑な設定が可能 | 高速 |
| Antigen | シンプル、Oh-My-Zsh互換 | 中程度 |
| Oh-My-Zsh | 豊富なプラグイン、初心者向け | 低速 |

### 5.2 Sheldonの推奨設定

```toml
# sheldon/plugins.toml
shell = "zsh"

# 遅延読み込み用（先頭で定義）
[plugins.zsh-defer]
github = "romkatv/zsh-defer"
apply = ["source"]

# 必須プラグイン（最小限に抑える）
[plugins.zsh-syntax-highlighting]
github = "zsh-users/zsh-syntax-highlighting"

[plugins.zsh-completions]
github = "zsh-users/zsh-completions"
apply = ["source"]

[plugins.zsh-autosuggestions]
github = "zsh-users/zsh-autosuggestions"
```

**パフォーマンスのポイント**:
- プラグインは必要最小限に（3-5個程度が理想）
- `zsh-defer`を使用した遅延読み込みを検討
- 定義順序が読み込み順序に影響する

### 5.3 遅延読み込みの設定例

```toml
[templates]
defer = { value = 'zsh-defer source "{{ file }}"', each = true }

[plugins.slow-plugin]
github = "example/slow-plugin"
apply = ["defer"]
```

## 6. パフォーマンス最適化

### 6.1 起動時間の計測

```zsh
# 起動時間計測
time zsh -i -c exit

# 詳細なプロファイリング
zmodload zsh/zprof
# ... 設定読み込み後 ...
zprof
```

### 6.2 最適化テクニック

| テクニック | 説明 |
|------------|------|
| 遅延読み込み | `zsh-defer`でプラグインを遅延読み込み |
| 補完キャッシュ | `compinit -C`でキャッシュを活用 |
| 条件付き読み込み | コマンド存在確認後に設定を読み込み |
| プラグイン最小化 | 本当に必要なプラグインのみ使用 |

### 6.3 条件付き読み込みの例

```zsh
# コマンドが存在する場合のみ設定
if (( $+commands[go] )); then
    export PATH="$(go env GOPATH)/bin:$PATH"
fi

# または
if which go >/dev/null 2>&1; then
    export PATH="$(go env GOPATH)/bin:$PATH"
fi
```

## 7. 現在の設定の評価

### 7.1 良い点

- **モジュール化**: `.zsh/`ディレクトリで設定を分離
- **Sheldon採用**: 高速なプラグインマネージャー
- **重複パス防止**: `typeset -U`を使用
- **プラグイン最小化**: 必要最低限のプラグインのみ使用
- **プラットフォーム対応**: macOS/Linux両対応

### 7.2 改善の余地

| 項目 | 現状 | 推奨 |
|------|------|------|
| 履歴設定の配置 | `.zshenv`と`.zsh/opt.zsh`に分散 | 一箇所にまとめる |
| `compinit`の呼び出し | `.zshenv`で実行 | `.zshrc`で実行が一般的 |
| XDG準拠 | 非準拠 | 必要に応じて検討 |
| 起動時間計測 | なし | プロファイリング追加を検討 |

## 8. セキュリティ考慮事項

- **シークレット管理**: `.gitconfig.local`のように、機密情報は別ファイルに分離しgitignore
- **実行権限**: シェルスクリプトの権限を適切に設定
- **PATH設定**: 信頼できるディレクトリのみPATHに追加

## 9. 参考資料

### 公式ドキュメント
- [Zsh公式ドキュメント - Startup Files](https://zsh.sourceforge.io/Intro/intro_3.html)
- [Zsh Guide - Chapter 2](https://zsh.sourceforge.io/Guide/zshguide02.html)
- [Sheldon公式ドキュメント](https://sheldon.cli.rs/)

### 解説記事
- [Zsh Configuration Files | Baeldung](https://www.baeldung.com/linux/zsh-configuration-files)
- [How Do Zsh Configuration Files Work? | freeCodeCamp](https://www.freecodecamp.org/news/how-do-zsh-configuration-files-work/)
- [Zsh/Bash startup files loading order | Medium](https://medium.com/@rajsek/zsh-bash-startup-files-loading-order-bashrc-zshrc-etc-e30045652f2e)
- [XDG Base Directory | ArchWiki](https://wiki.archlinux.org/title/XDG_Base_Directory)
- [Zsh | ArchWiki](https://wiki.archlinux.org/title/Zsh)

### dotfiles参考リポジトリ
- [thoughtbot/dotfiles](https://github.com/thoughtbot/dotfiles)
- [z0rc/dotfiles](https://github.com/z0rc/dotfiles)
- [dotfiles.github.io](https://dotfiles.github.io/inspiration/)

### 2025年の記事
- [My Updated ZSH Config 2025 | Scott Spence](https://scottspence.com/posts/my-updated-zsh-config-2025)
- [The best minimal zsh configuration | Felipe Contreras](https://felipec.wordpress.com/2025/01/20/zsh-min/)
