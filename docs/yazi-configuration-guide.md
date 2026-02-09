# Yazi Configuration Guide

## 概要

yaziは、Rustで書かれた高速なターミナルファイルマネージャーです。このドキュメントでは、dotfilesリポジトリにおけるyazi導入の経緯、設定方法、トラブルシューティングをまとめます。

## バージョン情報

- **yazi**: 26.1.4 (Homebrew 2026-01-04)
- **設定ファイル**: `~/.config/yazi/`
- **dotfiles管理**: `~/.dotfiles/yazi/` → `~/.config/yazi/` (シンボリックリンク)

## 導入の経緯

### 既存ツールとの棲み分け

- **fzf**: コマンド履歴検索（Ctrl+r、頻度順）、ディレクトリジャンプ（Ctrl+s）、ghqリポジトリ選択（`gf`コマンド）
- **yazi**: ファイルブラウジング、プレビュー（画像・Markdown・コード）、ファイル操作、ディレクトリツリー表示

### デザイン哲学

- **Emacs風キーバインド**: Ctrl+n/p/f/bでのカーソル移動
- **yazi標準のファイル操作**: y（ヤンク）、p（ペースト）、d（削除）、r（リネーム）
- **Tokyo Nightテーマ**: ghosttyとの視覚的統一
- **ghq連携**: yaziからghqリポジトリを選択・移動

## 設定ファイル構造

```
~/.dotfiles/yazi/
├── yazi.toml          # メイン設定
├── keymap.toml        # キーバインド
└── theme.toml         # Tokyo Nightテーマ
```

## Version 26の重要な変更点

### キーマップ構文の変更

yaziのバージョン26では、キーマップの設定方法が大きく変更されました。

#### ❌ 誤った設定（古い構文）

```toml
[manager]
keymap = [
    { on = [ "<C-p>" ], run = "arrow -1", desc = "Move cursor up" },
]
```

#### ✅ 正しい設定（Version 26）

```toml
[mgr]
prepend_keymap = [
    { on = "<C-p>", run = "arrow -1", desc = "Move cursor up" },
]
```

### 主要な変更点

1. **セクション名**: `[manager]` → `[mgr]`
2. **属性名**: `keymap` → `prepend_keymap` または `append_keymap`
3. **キー表記**:
   - 単一キー: `on = "<C-p>"` （文字列形式）
   - 複数キーシーケンス: `on = [ "<C-x>", "<C-c>" ]` （配列形式）

### セクション一覧

| セクション名 | 用途 |
|------------|------|
| `[mgr]` | ファイルマネージャーのメイン操作 |
| `[tasks]` | タスクマネージャー（バックグラウンド処理） |
| `[select]` | 選択モード |
| `[input]` | 入力フィールド（検索、リネームなど） |
| `[help]` | ヘルプ表示 |

### prepend_keymap vs append_keymap

- **prepend_keymap**: デフォルトキーバインドよりも優先される（推奨）
- **append_keymap**: デフォルトキーバインドの後に追加される
- **keymap**: デフォルトを完全に上書き（非推奨）

## Emacs風キーバインド設定

### 基本移動（yazi/keymap.toml）

```toml
[mgr]
prepend_keymap = [
    # 基本移動（Emacs風）
    { on = "<C-p>", run = "arrow -1", desc = "Move cursor up (previous)" },
    { on = "<C-n>", run = "arrow 1", desc = "Move cursor down (next)" },
    { on = "<C-b>", run = "leave", desc = "Go back to parent directory (backward)" },
    { on = "<C-f>", run = "enter", desc = "Enter directory (forward)" },

    # 行頭・行末移動
    { on = "<C-a>", run = "arrow -99999999", desc = "Move to first item" },
    { on = "<C-e>", run = "arrow 99999999", desc = "Move to last item" },

    # ページスクロール
    { on = "<C-v>", run = "arrow 50%", desc = "Scroll down half page" },
    { on = "<A-v>", run = "arrow -50%", desc = "Scroll up half page" },

    # 検索
    { on = "<C-s>", run = "find", desc = "Find file (search forward)" },
    { on = "<C-r>", run = "find --previous", desc = "Find previous (search reverse)" },

    # キャンセル
    { on = "<C-g>", run = "escape", desc = "Cancel / Escape" },

    # 終了
    { on = [ "<C-x>", "<C-c>" ], run = "quit", desc = "Quit Emacs style (save cwd)" },
]
```

### inputモードでのEmacs編集（yazi/keymap.toml）

```toml
[input]
prepend_keymap = [
    { on = "<C-g>", run = "close", desc = "Cancel input (Emacs style)" },
    { on = "<C-a>", run = "move -999", desc = "Move to beginning of line" },
    { on = "<C-e>", run = "move 999", desc = "Move to end of line" },
    { on = "<C-f>", run = "move 1", desc = "Move forward one character" },
    { on = "<C-b>", run = "move -1", desc = "Move backward one character" },
    { on = "<C-k>", run = "kill eof", desc = "Kill to end of line" },
    { on = "<C-u>", run = "kill bol", desc = "Kill to beginning of line" },
]
```

### ghosttyスプリット移動との共存

| キーバインド | 動作 | 処理レベル |
|------------|------|----------|
| Ctrl+n/p/f/b | yaziのカーソル移動 | アプリケーションレベル（yazi内のみ） |
| Ctrl+Shift+n/p/f/b | ghosttyのスプリット移動 | ターミナルレベル（yazi起動中でも機能） |

**動作例**:
```
# yazi起動中
Ctrl+n       → yaziのカーソル下移動
Ctrl+Shift+n → ghosttyの下スプリットへ移動（yaziとは無関係）
```

## Tokyo Nightテーマ設定

### カラーパレット（yazi/theme.toml）

```toml
# Tokyo Night テーマ for Yazi

[manager]
cwd = { fg = "#7aa2f7", bold = true }

[filetype]
rules = [
    # ディレクトリ
    { name = "*/", fg = "#7aa2f7" },

    # 実行ファイル
    { name = "*", is = "exec", fg = "#f7768e" },

    # シンボリックリンク
    { name = "*", is = "link", fg = "#7dcfff" },

    # 隠しファイル
    { name = ".*", fg = "#626880" },

    # ドキュメント
    { name = "*.md", fg = "#9ece6a" },
    { name = "*.txt", fg = "#9ece6a" },

    # コード
    { name = "*.ts", fg = "#7aa2f7" },
    { name = "*.js", fg = "#e0af68" },
    { name = "*.py", fg = "#7dcfff" },
    { name = "*.rb", fg = "#f7768e" },

    # デフォルト
    { name = "*", fg = "#a9b1d6" },
]
```

### カラーコード一覧

| 用途 | カラー名 | Hex値 | 例 |
|-----|---------|-------|---|
| プライマリ | Blue | `#7aa2f7` | ディレクトリ、TypeScript |
| 強調 | Red/Pink | `#f7768e` | 実行ファイル、Ruby |
| セカンダリ | Purple | `#bb9af7` | Rust |
| 成功 | Green | `#9ece6a` | ドキュメント |
| 警告 | Yellow/Gold | `#e0af68` | 画像、JavaScript |
| 情報 | Cyan | `#7dcfff` | シンボリックリンク、Python |
| 背景 | Dark Blue | `#1a1b26` | メイン背景 |
| テキスト | Light Gray | `#a9b1d6` | デフォルトテキスト |
| テキスト2 | Medium Gray | `#626880` | 隠しファイル |

## メイン設定（yazi.toml）

### emacsclient統合

```toml
[opener]
edit = [
    { run = 'emacsclient -c "$@"', block = true, for = "macos" },
    { run = 'code "$@"', block = false, for = "macos" },
]
open = [
    { run = 'open "$@"', desc = "Open with default app", for = "macos" },
]
```

### プレビュー設定

```toml
[preview]
tab_size = 2
max_width = 600
max_height = 900
cache_dir = ""
image_filter = "lanczos3"
image_quality = 90
```

## Zsh統合（.zsh/yazi.zsh）

### cd on quit機能

```bash
# yaziで移動したディレクトリにシェルも移動
function y() {
    local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
    command yazi "$@" --cwd-file="$tmp"
    if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
        builtin cd -- "$cwd"
    fi
    rm -f -- "$tmp"
}
```

**動作**:
- `q`: yaziで移動したディレクトリにシェルも移動（cwd保存）
- `Q`: シェルは元のディレクトリのまま（cwd保存なし）

### ghq連携

```bash
# ghqリポジトリルートでyaziを起動
function yg() {
    if ! which ghq > /dev/null 2>&1; then
        echo "Error: ghq is not installed"
        return 1
    fi

    local ghq_root="$(ghq root)"
    if [ -n "$ghq_root" ]; then
        y "$ghq_root"
    else
        echo "Error: ghq root not configured"
        return 1
    fi
}

# ghqリポジトリ一覧からfzyで選択してyaziで開く
function ygh() {
    if ! which ghq > /dev/null 2>&1; then
        echo "Error: ghq is not installed"
        return 1
    fi

    if ! which fzf > /dev/null 2>&1; then
        echo "Error: fzf is not installed"
        return 1
    fi

    local repo=$(ghq list | fzf)
    if [ -n "$repo" ]; then
        y "$(ghq root)/$repo"
    fi
}
```

**コマンド一覧**:
- `y`: yaziを起動（cd on quit付き）
- `yg`: ghqルートディレクトリでyazi起動
- `ygh`: fzyでリポジトリ選択 → yazi起動

## トラブルシューティング

### キーバインドが動作しない

#### 症状
Ctrl+n/pなどのキーバインドが反応しない。

#### 原因と対処法

1. **構文エラー**: セクション名が`[manager]`になっている
   ```toml
   # ❌ 誤り
   [manager]

   # ✅ 正しい
   [mgr]
   ```

2. **属性名エラー**: `keymap`を使用している
   ```toml
   # ❌ 誤り
   keymap = [...]

   # ✅ 正しい
   prepend_keymap = [...]
   ```

3. **キー表記エラー**: 単一キーが配列形式になっている
   ```toml
   # ❌ 誤り
   { on = [ "<C-p>" ], run = "arrow -1" }

   # ✅ 正しい
   { on = "<C-p>", run = "arrow -1" }
   ```

4. **キャッシュの問題**: 設定変更後にキャッシュをクリア
   ```bash
   yazi --clear-cache
   ```

### テーマが適用されない

#### 原因と対処法

1. **theme.tomlの構文エラー**: TOMLバリデーターで確認
   ```bash
   # オンラインバリデーター
   # https://taplo.tamasfe.dev/
   ```

2. **キャッシュクリア**:
   ```bash
   yazi --clear-cache
   yazi  # 再起動
   ```

### cd on quitが動作しない

#### 原因と対処法

1. **y関数が定義されていない**: `.zsh/yazi.zsh`が読み込まれているか確認
   ```bash
   type y
   # 期待結果: y is a shell function from ...
   ```

2. **設定の再読み込み**:
   ```bash
   source ~/.zshrc
   ```

### ghq連携が動作しない

#### 原因と対処法

1. **ghqがインストールされていない**:
   ```bash
   which ghq
   # 期待結果: /opt/homebrew/bin/ghq
   ```

2. **ghq rootが設定されていない**:
   ```bash
   ghq root
   # 期待結果: /Users/username/ghq
   ```

3. **fzfがインストールされていない**（`ygh`コマンドのみ）:
   ```bash
   which fzf
   # 期待結果: /opt/homebrew/bin/fzf
   ```

## 検証コマンド

### 基本機能確認

```bash
# バージョン確認
yazi --version
# 期待結果: Yazi 26.1.4 (Homebrew 2026-01-04)

# 設定ファイル確認
ls -la ~/.config/yazi/
# 期待結果: yazi.toml, keymap.toml, theme.toml が存在

# シンボリックリンク確認
ls -la ~/.config/yazi
# 期待結果: ~/.config/yazi -> /Users/username/.dotfiles/yazi
```

### キーバインド確認

```bash
# yaziを起動
y

# 以下のキーを試す:
# Ctrl+n/p: 上下移動
# Ctrl+f: ディレクトリに入る
# Ctrl+b: 親ディレクトリに戻る
# Ctrl+a/e: 先頭・末尾へ移動
# Ctrl+v: ページスクロール（下）
# Alt+v: ページスクロール（上）
# Ctrl+s: 検索（前方）
# Ctrl+r: 検索（後方）
# Ctrl+g: キャンセル
# q: 終了（cd on quit）
# Q: 終了（cd on quitなし）
```

### cd on quit確認

```bash
# 現在のディレクトリ確認
pwd  # 例: /Users/username

# yaziで別ディレクトリに移動して`q`で終了
y
# (yaziで ~/Documents に移動)
# (q を押す)

pwd  # 期待結果: /Users/username/Documents
```

### ghq連携確認

```bash
# ghqルートでyazi起動
yg
# 期待結果: ghqルートディレクトリ（例: ~/ghq）でyaziが起動

# fzyでリポジトリ選択 → yazi起動
ygh
# 期待結果: fzyでリポジトリ選択画面表示
```

## 設定の拡張

### プレビュー機能の拡張

```bash
# PDFプレビュー
brew install poppler

# 動画サムネイル
brew install ffmpegthumbnailer
```

### プラグイン追加

yaziは[awesome-yazi](https://github.com/AnirudhG07/awesome-yazi)から有用なプラグインを追加できます。

推奨プラグイン:
- **jump.yazi**: autojump/z風のディレクトリジャンプ
- **bookmarks.yazi**: ブックマーク機能強化
- **chmod.yazi**: ファイル権限変更

## 参考リンク

### 公式ドキュメント
- [keymap.toml | Yazi](https://yazi-rs.github.io/docs/configuration/keymap/)
- [yazi.toml | Yazi](https://yazi-rs.github.io/docs/configuration/yazi/)
- [theme.toml | Yazi](https://yazi-rs.github.io/docs/configuration/theme/)
- [Configuration | Yazi](https://yazi-rs.github.io/docs/configuration/overview/)

### GitHub
- [sxyazi/yazi](https://github.com/sxyazi/yazi) - Yazi公式リポジトリ
- [Default keymap.toml](https://github.com/sxyazi/yazi/blob/shipped/yazi-config/preset/keymap-default.toml)
- [How to make keymap.toml work?](https://github.com/sxyazi/yazi/discussions/2844)

### チュートリアル
- [How To Use The Amazing & Fast Yazi Terminal File Manager](https://www.josean.com/posts/how-to-use-yazi-file-manager)
- [Yazi Keyboard Shortcuts Quick Reference](https://kb.adamsdesk.com/application/yazi-keyboard-shortcuts/)

## まとめ

yaziはVersion 26で設定構文が大きく変更されました。特に以下の点に注意が必要です：

1. **セクション名**: `[mgr]`（`[manager]`ではない）
2. **属性名**: `prepend_keymap`（`keymap`ではない）
3. **キー表記**: 単一キーは文字列形式（配列ではない）

これらの規則に従えば、Emacs風のキーバインドとyazi標準のファイル操作を組み合わせた快適なファイル管理環境を構築できます。
