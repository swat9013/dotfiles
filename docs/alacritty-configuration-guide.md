# Alacritty 設定ガイド

> 公式ドキュメントに基づくAlacritty設定リファレンス

## 目次

1. [基礎知識](#1-基礎知識)
2. [設定ファイルの構成](#2-設定ファイルの構成)
3. [フォント設定](#3-フォント設定)
4. [テーマ・カラー設定](#4-テーマカラー設定)
5. [キーバインド設定](#5-キーバインド設定)
6. [ウィンドウ設定](#6-ウィンドウ設定)
7. [カーソル設定](#7-カーソル設定)
8. [ターミナル・シェル設定](#8-ターミナルシェル設定)
9. [ヒント機能](#9-ヒント機能)
10. [マウス設定](#10-マウス設定)
11. [IPC機能](#11-ipc機能)
12. [tmux連携](#12-tmux連携)
13. [マルチプラットフォーム対応](#13-マルチプラットフォーム対応)
14. [推奨設定テンプレート](#14-推奨設定テンプレート)
15. [トラブルシューティング](#15-トラブルシューティング)
16. [既知の制限事項](#16-既知の制限事項)

---

## 1. 基礎知識

### 1.1 概要

Alacrittyは、GPU アクセラレーションを活用した高速なクロスプラットフォームターミナルエミュレータ。タブやスプリットなどの機能は意図的に省略されており、ウィンドウマネージャやtmuxとの組み合わせを推奨。

- **最新バージョン**: 0.16.1（2025年10月）
- **対応プラットフォーム**: Linux, BSD, macOS, Windows
- **ライセンス**: Apache 2.0 / MIT（デュアルライセンス）
- **最小Rustバージョン**: 1.85.0（v0.16.0以降）

### 1.5 v0.16.0 の主要な新機能

- **Unicode 17サポート**: 最新のシンボルとスクリプトへの対応拡大
- **Vi mode新機能**: `*`, `#`, `{`, `}` モーション、`Y`（行末までyank）
- **システムワイド設定**: `/etc/alacritty/alacritty.toml` フォールバック対応
- **IPC機能拡張**: `alacritty msg get-config` で現在の設定を取得可能
- **box-drawing改善**: 角丸のビルトイン描画によるクリーンなレンダリング
- **macOS**: `~/.zshenv` の自動ソースを停止（`.zprofile`に環境変数を移動推奨）

### 1.2 設定ファイルの場所

Alacrittyは設定ファイルを自動作成しない。以下の順序で検索される。

```bash
# Linux/BSD
$XDG_CONFIG_HOME/alacritty/alacritty.toml
$XDG_CONFIG_HOME/alacritty.toml
~/.config/alacritty/alacritty.toml
~/.alacritty.toml

# macOS
~/.config/alacritty/alacritty.toml
~/.alacritty.toml

# Windows
%APPDATA%\alacritty\alacritty.toml
```

### 1.3 設定形式

**バージョン0.13.0以降**: TOML形式を使用。YAMLは非推奨。

```bash
# YAMLからTOMLへの移行
alacritty migrate --dry-run  # プレビュー
alacritty migrate            # 実行
```

> **注意**: 自動移行ではコメントが削除される。

### 1.4 設定のライブリロード

`live_config_reload` がデフォルトで有効。設定ファイル保存時に大部分の設定が即座に反映される。

```toml
[general]
live_config_reload = true  # デフォルト
```

---

## 2. 設定ファイルの構成

### 2.1 インポート機能

複数の設定ファイルを読み込み、モジュール分割が可能。

```toml
[general]
import = [
    "~/.config/alacritty/themes/tokyo-night.toml",
    "~/.config/alacritty/keybindings.toml",
]
```

- インポートは定義順に読み込まれる
- 同じキーは後から読み込まれた値で上書き
- 絶対パス（`/`）、ホームディレクトリ相対（`~/`）、設定ファイル相対パスに対応

### 2.2 推奨ディレクトリ構成

```
~/.config/alacritty/
├── alacritty.toml      # メイン設定
├── keybindings.toml    # キーバインド
└── themes/
    ├── tokyo-night.toml
    └── catppuccin-mocha.toml
```

### 2.3 設定セクション一覧

| セクション | 説明 |
|-----------|------|
| `[general]` | インポート、作業ディレクトリ、ライブリロード |
| `[env]` | 環境変数 |
| `[window]` | ウィンドウサイズ、装飾、透明度 |
| `[scrolling]` | スクロールバック履歴 |
| `[font]` | フォントファミリー、サイズ |
| `[colors]` | カラースキーム |
| `[bell]` | ベル設定 |
| `[selection]` | テキスト選択 |
| `[cursor]` | カーソルスタイル |
| `[terminal]` | シェル、OSC52 |
| `[mouse]` | マウス設定 |
| `[hints]` | URLヒント |
| `[keyboard]` | キーバインド |

---

## 3. フォント設定

### 3.1 基本設定

```toml
[font]
size = 14.0

[font.normal]
family = "JetBrains Mono"
style = "Regular"

[font.bold]
family = "JetBrains Mono"
style = "Bold"

[font.italic]
family = "JetBrains Mono"
style = "Italic"

[font.bold_italic]
family = "JetBrains Mono"
style = "Bold Italic"
```

### 3.2 プラットフォームデフォルト

| プラットフォーム | デフォルトフォント |
|----------------|------------------|
| Linux/BSD | `monospace` |
| macOS | `Menlo` |
| Windows | `Consolas` |

### 3.3 フォント調整

```toml
[font]
size = 11.25  # デフォルト

# グリフ位置調整
[font.offset]
x = 0
y = 0

[font.glyph_offset]
x = 0
y = 0

# ビルトインボックス描画文字（Powerlineシンボル等）
builtin_box_drawing = true
```

### 3.4 Nerd Fonts の設定

Nerd Fontsはアイコンやシンボルが埋め込まれたパッチ済みフォント。ターミナルツール（Starship, Powerlevel10k, yazi等）で広く使われる。

```bash
# macOS でのインストール
brew install font-jetbrains-mono-nerd-font
brew install font-meslo-lg-nerd-font
```

```toml
[font.normal]
family = "JetBrainsMono Nerd Font"  # ファイル名ではなく登録名を使用
style = "Regular"
```

> **注意**: フォント名はファイル名（`JetBrainsMonoNerdFont-Regular.ttf`）ではなく、システムに登録される名前を使用する。`fc-list | grep "JetBrains"` またはmacOSの Font Book で確認。

### 3.5 フォントフォールバック

Alacrittyは明示的なフォールバックリストをサポートしていない。フォールバックが必要な場合はfontconfigで設定する。

```xml
<!-- ~/.config/fontconfig/fonts.conf -->
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <alias>
    <family>JetBrainsMono Nerd Font</family>
    <prefer>
      <family>Noto Color Emoji</family>
      <family>Symbols Nerd Font</family>
    </prefer>
  </alias>
</fontconfig>
```

### 3.6 リガチャについて

Alacrittyは**フォントリガチャを公式サポートしていない**。これはパフォーマンス重視の設計方針による。レンダリングが文字単位（`RenderableCell`）で行われるため、リガチャに必要な文字列単位（`TextRun`）の処理が含まれていない。

リガチャが必要な場合の代替:
- **WezTerm**: Rust製、リガチャサポート、クロスプラットフォーム
- **Kitty**: GPU加速、リガチャサポート

---

## 4. テーマ・カラー設定

### 4.1 公式テーマリポジトリの使用

```bash
# テーマをクローン
mkdir -p ~/.config/alacritty/themes
git clone https://github.com/alacritty/alacritty-theme ~/.config/alacritty/themes
```

```toml
# alacritty.toml
[general]
import = [
    "~/.config/alacritty/themes/themes/tokyo_night.toml"
]
```

### 4.2 人気テーマ

| テーマ | 特徴 |
|-------|------|
| Tokyo Night | モダンダークテーマ。Storm、Enhanced変種あり |
| Dracula | 鮮やかな紫系ダークテーマ |
| Catppuccin | パステル調。Mocha/Macchiato/Frappe/Latte の4変種 |
| Nord | 北極風の青系パレット |
| Gruvbox | レトロカラー。Dark/Light変種 |
| One Dark | Atom由来の人気テーマ |
| Solarized | 科学的に設計された色温度。Dark/Light |
| Rose Pine | 3変種（Rose Pine、Moon、Dawn） |
| GitHub | アクセシビリティ重視。色覚異常対応変種あり |
| Kanagawa | 日本画風の落ち着いたカラーパレット |

### 4.3 テーマ管理CLIツール

`alacritty-themes` でインタラクティブにテーマを切り替え可能。

```bash
npm install -g alacritty-themes
alacritty-themes              # インタラクティブ選択
alacritty-themes Dracula      # テーマ名を直接指定
```

### 4.4 IPC による動的テーマ切り替え

```bash
# 現在のウィンドウに適用
alacritty msg config "$(cat ~/.config/alacritty/themes/themes/nord.toml)"

# 全ウィンドウに適用
alacritty msg config -1 "$(cat ~/.config/alacritty/themes/themes/nord.toml)"
```

### 4.5 カスタムカラー設定

```toml
[colors.primary]
foreground = "#d8d8d8"
background = "#181818"
dim_foreground = "#828482"
bright_foreground = "#d8d8d8"

[colors.cursor]
text = "CellBackground"      # 特殊値: セル背景色を使用
cursor = "CellForeground"    # 特殊値: セル前景色を使用

[colors.vi_mode_cursor]
text = "CellBackground"
cursor = "CellForeground"

[colors.selection]
text = "CellBackground"
background = "CellForeground"

# 検索ハイライト
[colors.search.matches]
foreground = "#181818"
background = "#ac4242"

[colors.search.focused_match]
foreground = "#181818"
background = "#f4bf75"

# ヒント
[colors.hints.start]
foreground = "#181818"
background = "#f4bf75"

[colors.hints.end]
foreground = "#181818"
background = "#ac4242"

# フッターバー
[colors.footer_bar]
foreground = "#181818"
background = "#d8d8d8"
```

### 4.6 ANSIカラーパレット

```toml
[colors.normal]
black   = "#1a1b26"
red     = "#f7768e"
green   = "#9ece6a"
yellow  = "#e0af68"
blue    = "#7aa2f7"
magenta = "#bb9af7"
cyan    = "#7dcfff"
white   = "#a9b1d6"

[colors.bright]
black   = "#414868"
red     = "#f7768e"
green   = "#9ece6a"
yellow  = "#e0af68"
blue    = "#7aa2f7"
magenta = "#bb9af7"
cyan    = "#7dcfff"
white   = "#c0caf5"

# オプション: dimカラー
[colors.dim]
black   = "#1a1b26"
red     = "#f7768e"
# ...

# カスタムインデックスカラー（16-256）
[[colors.indexed_colors]]
index = 16
color = "#ff9e64"
```

### 4.7 追加オプション

```toml
[colors]
# セル背景にも透明度を適用
transparent_background_colors = false

# ボールドテキストを明るい色で描画
draw_bold_text_with_bright_colors = false
```

---

## 5. キーバインド設定

### 5.1 基本構文

```toml
[[keyboard.bindings]]
key = "C"
mods = "Control|Shift"
action = "Copy"

[[keyboard.bindings]]
key = "V"
mods = "Control|Shift"
action = "Paste"
```

### 5.2 修飾キー

複数の修飾キーは `|` で結合。

| 修飾キー | 説明 |
|---------|------|
| `Control` | Ctrl |
| `Shift` | Shift |
| `Alt` | Alt (Option on macOS) |
| `Command` | Cmd (macOS only) |

### 5.3 モード制約

```toml
[[keyboard.bindings]]
key = "PageUp"
mods = "Shift"
mode = "~Alt"        # Altモードでない場合のみ有効
action = "ScrollPageUp"

[[keyboard.bindings]]
key = "Escape"
mode = "Vi|Search"   # ViモードまたはSearchモードで有効
action = "ToggleViMode"
```

### 5.4 主要アクション一覧

**ナビゲーション**
```toml
action = "ScrollPageUp"
action = "ScrollPageDown"
action = "ScrollToTop"
action = "ScrollToBottom"
```

**編集**
```toml
action = "Copy"
action = "Paste"
action = "ClearSelection"
```

**Viモード**
```toml
action = "ToggleViMode"
action = "InlineSearchForward"
action = "SemanticSearchForward"
```

**検索**
```toml
action = "SearchForward"
action = "SearchBackward"
action = "SearchNext"
```

**ウィンドウ**
```toml
action = "ToggleFullscreen"
action = "CreateNewWindow"
action = "SpawnNewInstance"
```

**フォントサイズ**
```toml
action = "IncreaseFontSize"
action = "DecreaseFontSize"
action = "ResetFontSize"
```

### 5.5 テキスト送信

```toml
[[keyboard.bindings]]
key = "N"
mods = "Control"
chars = "\u001b[13;5u"  # Unicode エスケープシーケンス
```

### 5.6 プラットフォーム別デフォルト

**Windows/Linux/BSD**
- `Ctrl+Shift+C`: Copy
- `Ctrl+Shift+V`: Paste
- `Ctrl+0`: ResetFontSize

**macOS**
- `Cmd+C`: Copy
- `Cmd+V`: Paste
- `Cmd+0`: ResetFontSize

### 5.7 Viモード

`Ctrl+Shift+Space` でViモードを切り替え。

**ナビゲーション**
- `h/j/k/l`: 左/下/上/右
- `w/b`: 単語移動（前方/後方）
- `0/$`: 行頭/行末
- `gg/G`: バッファの先頭/末尾
- `{/}`: 段落移動（v0.16.0+）

**テキスト選択**
- `v`: カーソル位置から選択開始
- `V` (Shift+v): 行選択
- `Alt+v`: セマンティック選択（単語、URL等）
- `Ctrl+v`: ブロック選択

**コピー**
- `y`: 選択範囲をクリップボードにコピー
- `Y`: 行末までyank（v0.16.0+）

**検索**
- `/`: 前方検索
- `?`: 後方検索
- `n/N`: 次/前の一致へ移動
- `*`: カーソル下の単語を前方検索（v0.16.0+）
- `#`: カーソル下の単語を後方検索（v0.16.0+）

---

## 6. ウィンドウ設定

### 6.1 サイズと位置

```toml
[window]
# サイズ（セル単位、0で自動）
[window.dimensions]
columns = 120
lines = 35

# 位置（ピクセル、Waylandでは無効）
[window.position]
x = 100
y = 100
```

### 6.2 パディング

```toml
[window]
# 内側余白（ピクセル）
[window.padding]
x = 8
y = 8

# 追加パディングを均等配分
dynamic_padding = false
```

### 6.3 装飾と透明度

```toml
[window]
# 装飾スタイル: Full, None, Transparent, Buttonless
decorations = "Full"

# 背景透明度（0.0-1.0）
opacity = 1.0

# 背景ブラー（対応コンポジタのみ）
blur = false
```

### 6.4 起動モード

```toml
[window]
# Windowed, Maximized, Fullscreen, SimpleFullscreen
startup_mode = "Windowed"
```

### 6.5 タイトル

```toml
[window]
title = "Alacritty"
dynamic_title = true  # アプリによる変更を許可
```

### 6.6 プラットフォーム固有

```toml
[window]
# macOS: Optionキーの動作
# None, OnlyLeft, OnlyRight, Both
option_as_alt = "None"

# ウィンドウレベル: Normal, AlwaysOnTop
level = "Normal"

# テーマバリアント: None, Dark, Light
decorations_theme_variant = "None"

# Linux/X11: ウィンドウクラス
[window.class]
instance = "Alacritty"
general = "Alacritty"
```

---

## 7. カーソル設定

### 7.1 スタイル

```toml
[cursor.style]
# Block, Underline, Beam
shape = "Block"

# Never, Off, On, Always
blinking = "Off"

# Viモード時のスタイル（None でデフォルト使用）
[cursor.vi_mode_style]
shape = "Block"
blinking = "Off"
```

### 7.2 ブリンク設定

```toml
[cursor]
blink_interval = 750     # ブリンク間隔（ミリ秒）
blink_timeout = 5        # 停止までの秒数（0で無効）
```

### 7.3 その他

```toml
[cursor]
unfocused_hollow = true  # 非フォーカス時に中空表示
thickness = 0.15         # カーソル幅（セル幅に対する割合、0.0-1.0）
```

---

## 8. ターミナル・シェル設定

### 8.1 シェル設定

```toml
[terminal]
# 文字列形式
shell = "/bin/zsh"

# オブジェクト形式（引数付き）
shell = { program = "/bin/zsh", args = ["-l"] }
```

**デフォルト**
- Linux/BSD/macOS: `$SHELL` または `/etc/passwd` のログインシェル
- Windows: `powershell`

### 8.2 tmuxとの連携例

```toml
[terminal]
shell = { program = "/opt/homebrew/bin/tmux", args = ["new-session", "-A", "-D", "-s", "main"] }
```

### 8.3 OSC 52（クリップボードアクセス）

```toml
[terminal]
# Disabled, OnlyCopy, OnlyPaste, CopyPaste
osc52 = "OnlyCopy"
```

---

## 9. ヒント機能

### 9.1 概要

ターミナル内のURL等を検出し、キーボードまたはマウスで操作可能にする。

### 9.2 デフォルトURL検出

```toml
[hints]
# ヒントラベルに使用する文字
alphabet = "jfkdls;ahgurieowpq"

[[hints.enabled]]
# URLパターン
regex = "(ipfs:|ipns:|magnet:|mailto:|gemini://|gopher://|https://|http://|news:|file:|git://|ssh:|ftp://)[^\u0000-\u001F\u007F-\u009F<>\"\\s{-}\\^⟨⟩`\\\\]+"

# 起動コマンド（プラットフォーム別）
command = "open"              # macOS
# command = "xdg-open"        # Linux/BSD
# command = { program = "cmd", args = ["/c", "start", ""] }  # Windows

hyperlinks = true
post_processing = true  # 末尾の句読点等を除去
persist = false
mouse.enabled = true
binding = { key = "O", mods = "Control|Shift" }
```

### 9.3 カスタムヒント例

```toml
# ファイル:行:列 形式をVS Codeで開く
[[hints.enabled]]
regex = "[^ ]+\\.rs:\\d+:\\d+"
command = { program = "code", args = ["--goto"] }
mouse = { enabled = true }
```

### 9.4 追加のカスタムヒント例

```toml
# Gitコミットハッシュをコピー
[[hints.enabled]]
regex = "[0-9a-f]{7,40}"
post_processing = false
action = "Copy"
binding = { key = "Y", mods = "Control|Shift" }

# ファイルパス検出
[[hints.enabled]]
regex = "(\\.?/[^\\s:]+)"
action = "Select"
binding = { key = "F", mods = "Control|Shift" }

# IPアドレスをコピー
[[hints.enabled]]
regex = "\\b(?:[0-9]{1,3}\\.){3}[0-9]{1,3}\\b"
action = "Copy"
binding = { key = "I", mods = "Control|Shift" }
```

**利用可能なアクション**:
- `Copy`: クリップボードにコピー
- `Paste`: ターミナルまたは検索にペースト
- `Select`: マッチ箇所を選択
- `MoveViModeCursor`: Vi modeカーソルを移動

---

## 10. マウス設定

### 10.1 基本設定

```toml
[mouse]
hide_when_typing = false
```

### 10.2 マウスバインディング

```toml
[[mouse.bindings]]
mouse = "Middle"
action = "PasteSelection"
```

---

## 11. IPC機能

### 11.1 概要

`alacritty msg` コマンドでプロセス間通信が可能。すべてのウィンドウが単一のAlacrittyインスタンスで動作する。

```toml
[general]
ipc_socket = true  # デフォルトで有効
```

### 11.2 新規ウィンドウ作成

```bash
alacritty msg create-window
alacritty msg create-window --working-directory ~/projects
alacritty msg create-window --title "Development"
```

### 11.3 設定取得（v0.16.0+）

```bash
alacritty msg get-config
```

### 11.4 動的設定変更

```bash
# 現在のウィンドウに適用
alacritty msg config "$(cat ~/path/to/theme.toml)"

# 全ウィンドウに適用
alacritty msg config -1 "$(cat ~/path/to/theme.toml)"
```

---

## 12. tmux連携

### 12.1 エスケープシーケンス方式

Alacrittyの `Cmd+Key` をtmuxが認識できるCSI（Control Sequence Introducer）コードに変換し、tmuxの `user-keys` にバインドする。

**仕組み**: Alacrittyでキー押下 → CSIコード送信 → tmuxが`user-keys`として受信 → アクション実行

### 12.2 Alacritty側の設定

```toml
# Cmd+D: 水平分割
[[keyboard.bindings]]
key = "D"
mods = "Command"
chars = "\u001b[25;6~"

# Cmd+Shift+D: 垂直分割
[[keyboard.bindings]]
key = "D"
mods = "Command|Shift"
chars = "\u001b[26;6~"

# Cmd+W: ペインを閉じる
[[keyboard.bindings]]
key = "W"
mods = "Command"
chars = "\u001b[28;6~"

# Cmd+T: 新しいウィンドウ
[[keyboard.bindings]]
key = "T"
mods = "Command"
chars = "\u001b[34;6~"

# Cmd+N: 新しいセッション
[[keyboard.bindings]]
key = "N"
mods = "Command"
chars = "\u001b[35;6~"

# Cmd+1〜9: ウィンドウ選択
[[keyboard.bindings]]
key = "Key1"
mods = "Command"
chars = "\u001b[36;6~"
# ... Key2〜Key9 は \u001b[37;6~ 〜 \u001b[44;6~
```

### 12.3 tmux側の設定

```bash
# user-keysの定義
set -s user-keys[0] "\e[25;6~"   # Cmd+D
set -s user-keys[1] "\e[26;6~"   # Cmd+Shift+D
set -s user-keys[5] "\e[28;6~"   # Cmd+W
set -s user-keys[7] "\e[34;6~"   # Cmd+T
set -s user-keys[8] "\e[35;6~"   # Cmd+N
set -s user-keys[9] "\e[36;6~"   # Cmd+1

# アクションのバインド（-n: プレフィックス不要）
bind -n User0 split-window -h -c "#{pane_current_path}"
bind -n User1 split-window -v -c "#{pane_current_path}"
bind -n User5 kill-pane
bind -n User7 new-window -c "#{pane_current_path}"
bind -n User8 new-session
bind -n User9 select-window -t 1
```

### 12.4 プレフィックスキー送信方式

tmuxのプレフィックスキー+コマンドキーをAlacrittyのキーバインドで送信する方式。

```toml
# Ctrl-a がプレフィックスの場合
# Cmd+T: 新タブ（プレフィックス + c）
[[keyboard.bindings]]
key = "T"
mods = "Command"
chars = "\x01\x63"

# Cmd+W: タブを閉じる（プレフィックス + d）
[[keyboard.bindings]]
key = "W"
mods = "Command"
chars = "\x01\x64"

# Cmd+]: 次のタブ（プレフィックス + n）
[[keyboard.bindings]]
key = "RBracket"
mods = "Command|Shift"
chars = "\x01\x6e"
```

> **Hex コード調査**: `xxd -psd` を実行し、キーを押して16進数コードを取得できる。

### 12.5 自動セッション接続

```toml
[terminal]
shell = { program = "/opt/homebrew/bin/tmux", args = ["new-session", "-A", "-D", "-s", "main"] }
```

`-A`: 既存セッションがあればアタッチ、`-D`: 他のクライアントをデタッチ

---

## 13. マルチプラットフォーム対応

### 13.1 プラットフォーム別設定の分離

Alacrittyはネイティブな条件分岐をサポートしていないため、シンボリックリンクで分離する。

```bash
# ~/.zshrc での切り替え
if [[ "$OSTYPE" == "darwin"* ]]; then
    ln -sf ~/.config/alacritty/platform/macos.toml ~/.config/alacritty/platform-local.toml
else
    ln -sf ~/.config/alacritty/platform/linux.toml ~/.config/alacritty/platform-local.toml
fi
```

```toml
# alacritty.toml
[general]
import = [
    "~/.config/alacritty/themes/themes/tokyo_night.toml",
    "~/.config/alacritty/platform-local.toml"
]
```

### 13.2 推奨ディレクトリ構成

```
~/.config/alacritty/
├── alacritty.toml           # メイン設定（共通 + import定義）
├── keybindings.toml         # キーバインド
├── platform/
│   ├── macos.toml           # macOS専用
│   └── linux.toml           # Linux専用
├── themes/                  # alacritty-theme クローン
│   └── themes/
│       ├── tokyo_night.toml
│       └── ...
└── local.toml               # マシン固有設定（git管理外）
```

### 13.3 macOS固有の設定

```toml
[window]
decorations = "Buttonless"              # タイトルバー非表示
option_as_alt = "Both"                  # OptionキーをAltとして使用
startup_mode = "SimpleFullscreen"       # macOSネイティブフルスクリーン
decorations_theme_variant = "Dark"
```

### 13.4 Linux固有の設定

```toml
# X11 ウィンドウクラス
[window.class]
instance = "Alacritty"
general = "Alacritty"

# Hints（xdg-open使用）
[[hints.enabled]]
regex = "(https?://|file://)[^\u0000-\u001F\u007F-\u009F<>\"\\s{-}\\^⟨⟩`]+"
command = "xdg-open"
mouse.enabled = true
```

> **注意**: Waylandでは `window.position` 設定が無効。`class.general` が `app_id` として使用される。

### 13.5 透明度のプラットフォーム差異

| プラットフォーム | 透明度サポート |
|----------------|---------------|
| macOS | ネイティブ対応 |
| Linux X11 | コンポジタ（picom等）が必要 |
| Linux Wayland | コンポジタ組み込み済み |
| Linux (EGL) | 動作しない可能性あり |

---

## 14. 推奨設定テンプレート

### 14.1 ミニマル設定

```toml
# ~/.config/alacritty/alacritty.toml
# ミニマル設定 - Alacrittyの優れたデフォルトを活かす

[general]
import = [
    "~/.config/alacritty/themes/themes/tokyo_night.toml"
]
live_config_reload = true

[font]
size = 14.0

[font.normal]
family = "JetBrains Mono"

[window.padding]
x = 8
y = 8
```

### 14.2 開発者向け設定

```toml
# ~/.config/alacritty/alacritty.toml
# 開発者向け最適化設定

[general]
import = [
    "~/.config/alacritty/themes/themes/tokyo_night.toml"
]
live_config_reload = true

[env]
TERM = "xterm-256color"

[window]
opacity = 0.95
dynamic_title = true

[window.dimensions]
columns = 140
lines = 40

[window.padding]
x = 12
y = 12

[scrolling]
history = 50000
multiplier = 5

[font]
size = 14.0
builtin_box_drawing = true

[font.normal]
family = "JetBrains Mono"
style = "Regular"

[cursor.style]
shape = "Block"
blinking = "Off"

[terminal]
shell = { program = "/bin/zsh", args = ["-l"] }
osc52 = "CopyPaste"

# キーバインド
[[keyboard.bindings]]
key = "N"
mods = "Command|Shift"
action = "SpawnNewInstance"

[[keyboard.bindings]]
key = "Return"
mods = "Command|Shift"
action = "ToggleFullscreen"

[[keyboard.bindings]]
key = "Equals"
mods = "Command"
action = "IncreaseFontSize"

[[keyboard.bindings]]
key = "Minus"
mods = "Command"
action = "DecreaseFontSize"

[[keyboard.bindings]]
key = "Key0"
mods = "Command"
action = "ResetFontSize"
```

### 14.3 macOS向け設定

```toml
# ~/.config/alacritty/alacritty.toml
# macOS最適化設定

[general]
import = [
    "~/.config/alacritty/themes/themes/catppuccin_mocha.toml"
]

[window]
decorations = "Buttonless"
opacity = 0.95
option_as_alt = "Both"

[window.padding]
x = 12
y = 8

[font]
size = 14.0

[font.normal]
family = "SF Mono"
style = "Regular"

[cursor.style]
shape = "Block"
blinking = "On"

[cursor]
blink_interval = 500
blink_timeout = 0
```

### 14.4 Linux Wayland向け設定

```toml
# ~/.config/alacritty/alacritty.toml
# Linux Wayland最適化設定

[general]
import = [
    "~/.config/alacritty/themes/themes/gruvbox_dark.toml"
]

[window]
decorations = "Full"
opacity = 0.9
blur = true

[window.dimensions]
columns = 120
lines = 35

[window.padding]
x = 16
y = 16

[scrolling]
history = 100000

[font]
size = 12.0

[font.normal]
family = "JetBrains Mono"
```

---

## 15. トラブルシューティング

### 15.1 一般的な問題

| 問題 | 解決策 |
|-----|-------|
| マウスがVimで動作しない | `.vimrc` に `set ttymouse=sgr` と `set mouse=a` を追加、またはNeovimを使用 |
| SSHでターミナル機能が壊れる | リモートサーバーにncursesをインストール、または `infocmp` と `tic` でterminfoをコピー |
| Wayland GNOMEでタイトルバー問題 | `WAYLAND_DISPLAY=` を空にしてXwaylandで起動 |
| 複数モニターでフォントサイズが異なる | `WINIT_X11_SCALE_FACTOR` 環境変数を設定（例: 1.66） |
| 透明度が効かない（Linux X11） | コンポジタ（picom等）を起動する |
| macOSで「悪意のあるソフトウェア」警告 | システム設定 → プライバシーとセキュリティ → 「このまま開く」 |

### 15.2 SSH先でのterminfo問題

```bash
# ワンライナーでリモートにterminfoをコピー
infocmp alacritty | ssh user@remote 'tic -x /dev/stdin'

# または環境変数で回避
[env]
TERM = "xterm-256color"
```

### 15.3 設定検証

```bash
# 設定ファイルの構文エラーを確認
alacritty --print-events 2>&1 | head -20

# 利用可能なフォントを確認
fc-list : family style | grep -i "mono"

# macOS: Font Bookアプリでフォント名を確認
```

### 15.4 YAML から TOML への移行

```bash
# プレビュー
alacritty migrate --dry-run

# 移行実行
alacritty migrate

# 特定ファイルを指定
alacritty migrate --config-file ~/.config/alacritty/alacritty.yml
```

> **注意**: 自動移行ではコメントが削除される。事前にバックアップ推奨。

---

## 16. 既知の制限事項

| 制限 | 説明 | 代替手段 |
|-----|------|---------|
| フォントリガチャ非対応 | パフォーマンス重視の設計方針 | WezTerm、Kitty |
| タブ機能なし | ミニマル設計 | tmux、ウィンドウマネージャ |
| 画像プロトコル非対応 | Sixel、Kitty画像プロトコル未実装 | Kitty、WezTerm |
| 明示的フォントフォールバック非対応 | fontconfig経由で設定 | Nerd Fontsパッチ済みフォント使用 |
| OSC 133（シェル統合）未実装 | feature request段階 | - |
| 条件分岐（OS判定）非対応 | 設定ファイル内での分岐不可 | シンボリックリンクで分離 |

---

## 参考リンク

### 公式

- [公式ドキュメント](https://alacritty.org/config-alacritty.html) - 設定リファレンス
- [キーバインドドキュメント](https://alacritty.org/config-alacritty-bindings.html) - キーバインド詳細
- [GitHub リポジトリ](https://github.com/alacritty/alacritty) - ソースコード
- [公式テーマコレクション](https://github.com/alacritty/alacritty-theme) - カラースキーム
- [alacritty msg マニュアル](https://alacritty.org/cmd-alacritty-msg.html) - IPC コマンド
- [Features ドキュメント](https://github.com/alacritty/alacritty/blob/master/docs/features.md) - 機能詳細

### コミュニティ

- [ArchWiki](https://wiki.archlinux.org/title/Alacritty) - 実践的なTips
- [キーボードマッピングWiki](https://github.com/alacritty/alacritty/wiki/Keyboard-mappings) - キーマッピング詳細

### 実践ガイド

- [Alacritty TOML and partial imports](https://shom.dev/posts/20240124_alacritty-toml-and-partial-imports/) - import機能の実用例
- [macOS Keyboard Shortcuts for tmux](https://www.joshmedeski.com/posts/macos-keyboard-shortcuts-for-tmux/) - tmux連携
- [Alacritty + tmux: Ghostty-like shortcuts](https://gist.github.com/ashwch/8861481ee002a10ad74ef66767918e50) - エスケープシーケンス方式
- [Git commit hash hints with Alacritty](https://bitfehler.srht.site/posts/2023-09-28_git-commit-hash-hints-with-alacritty.html) - カスタムHints

---

*最終更新: 2026年2月 - Alacritty 0.16.1対応*
