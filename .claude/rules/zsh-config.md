---
paths:
  - .zshenv
  - .zshrc
  - .zsh/**
---

# Zsh設定アーキテクチャ

## 起動ファイル読み込み順序

```
.zshenv → .zprofile → .zshrc → .zlogin → .zlogout
```

| ファイル | ログインシェル | 非ログインシェル | スクリプト |
|----------|---------------|-----------------|-----------|
| `.zshenv` | ○ | ○ | ○ |
| `.zprofile` | ○ | - | - |
| `.zshrc` | ○ | ○ | - |
| `.zlogin` | ○ (zshrc後) | - | - |
| `.zlogout` | ○ (終了時) | - | - |

### 各ファイルの役割

| ファイル | 配置すべき内容 |
|----------|---------------|
| `.zshenv` | 環境変数（EDITOR, LANG）、ZDOTDIR設定 |
| `.zprofile` | PATH設定（ログイン時のみ） |
| `.zshrc` | エイリアス、関数、プロンプト、キーバインド、補完 |
| `.zlogin` | ログイン時のみ実行するコマンド |
| `.zlogout` | クリーンアップ処理 |

**注意**: `.zshenv`は出力を生成するコマンドやTTY前提のコマンドを含めない

## 読み込みフロー（現在のdotfiles）

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

## モジュール化

### ファイル分割の指針

| カテゴリ | ファイル名 | 内容 |
|----------|-----------|------|
| オプション | `opt.zsh` | `setopt`による動作設定 |
| エイリアス | `aliases.zsh` | コマンドエイリアス |
| キーバインド | `keybinds.zsh` | `bindkey`設定 |
| 補完 | `completion.zsh` | `compinit`、`zstyle`設定 |

### 命名規則

- 拡張子: `.zsh`
- ファイル名: 小文字とアンダースコア
- 順序制御: 数字プレフィックス（`00-options.zsh`）

### 読み込みパターン

```zsh
for conf in $HOME/.dotfiles/.zsh/*.zsh; do
    source ${conf}
done
```

## プラグイン管理 (Sheldon)

**Oh-My-Zshではなく Sheldon を採用**（軽量・高速）

| プラグイン | 用途 |
|-----------|------|
| zsh-defer | 遅延読み込み、起動時間最適化 |
| zsh-syntax-highlighting | コマンド構文ハイライト |
| zsh-completions | 高度な補完機能 |
| ohmyzsh-lib | Oh-My-Zshのlib部分のみ使用 |
| pure | モダンプロンプト (非同期git情報) |

**設定ファイル**: `sheldon/plugins.toml` → `~/.config/sheldon/plugins.toml`

**ポイント**:
- プラグインは必要最小限に（3-5個程度が理想）
- `zsh-defer`を使用した遅延読み込みを検討
- 定義順序が読み込み順序に影響

## パフォーマンス最適化

### 起動時間の計測

```zsh
# 起動時間計測
time zsh -i -c exit

# 詳細プロファイリング
zmodload zsh/zprof
# ... 設定読み込み後 ...
zprof
```

### 最適化テクニック

| テクニック | 説明 |
|------------|------|
| 遅延読み込み | `zsh-defer`でプラグインを遅延読み込み |
| 補完キャッシュ | `compinit -C`でキャッシュを活用 |
| 条件付き読み込み | コマンド存在確認後に設定を読み込み |
| プラグイン最小化 | 本当に必要なプラグインのみ使用 |

### 条件付き読み込み

```zsh
if (( $+commands[go] )); then
    export PATH="$(go env GOPATH)/bin:$PATH"
fi
```

## プラットフォーム対応

### 検出パターン

```bash
if [ "$(uname)" = 'Darwin' ]; then
    # macOS固有設定
elif [ "$(expr substr $(uname -s) 1 5)" = 'Linux' ]; then
    # Linux固有設定
fi
```

### macOS固有設定

- Homebrew (M1/Intel対応)
- reattach-to-user-namespace (tmux clipboard)
- lib/macos.sh (input source設定)

### Linux固有設定

- Emacsenv PATH設定

## 自動更新 (lib/auto_update.sh)

24時間経過で自動 `git pull`:
1. `~/.last_update` タイムスタンプをチェック
2. HEAD vs origin/master 比較
3. 差分あれば自動pull
4. タイムスタンプ更新

## セキュリティ

- **シークレット管理**: `.gitconfig.local`のように、機密情報は別ファイルに分離しgitignore
- **PATH設定**: 信頼できるディレクトリのみPATHに追加

## 詳細ドキュメント

包括的なリファレンスは `docs/zsh-configuration-best-practices.md` を参照。
