# Ghostty 設定ガイドライン

> 徹底調査に基づく包括的な設定リファレンス＆ベストプラクティス集

## 目次

1. [基礎知識](#1-基礎知識)
2. [設定ファイルの構成](#2-設定ファイルの構成)
3. [フォント設定](#3-フォント設定)
4. [テーマ・カラー設定](#4-テーマカラー設定)
5. [キーバインド設定](#5-キーバインド設定)
6. [シェル統合](#6-シェル統合)
7. [ウィンドウ・外観設定](#7-ウィンドウ外観設定)
8. [パフォーマンス最適化](#8-パフォーマンス最適化)
9. [プラットフォーム固有設定](#9-プラットフォーム固有設定)
10. [推奨設定テンプレート](#10-推奨設定テンプレート)
11. [トラブルシューティング](#11-トラブルシューティング)

---

## 1. 基礎知識

### 1.1 設定ファイルの場所

```bash
# Linux / BSD
~/.config/ghostty/config
# または
$XDG_CONFIG_HOME/ghostty/config

# macOS
~/.config/ghostty/config
# または
~/Library/Application Support/com.mitchellh.ghostty/config
```

### 1.2 設定構文

```bash
# 基本構文: key = value
background = 282c34
foreground = ffffff

# コメントは # で開始（行頭のみ有効）
# これはコメントです

# 空白行は無視される

# 値のクォートは任意
font-family = "JetBrains Mono"
font-family = JetBrains Mono  # 同じ意味

# 空値でデフォルトにリセット
font-family =
```

### 1.3 設定の確認・検証

```bash
# 現在の設定を表示
ghostty +show-config

# デフォルト設定をドキュメント付きで表示
ghostty +show-config --default --docs | less

# 設定の検証
ghostty +validate-config

# 利用可能なテーマ一覧
ghostty +list-themes

# 利用可能なフォント一覧
ghostty +list-fonts

# 現在のキーバインド一覧
ghostty +list-keybinds

# 利用可能なアクション一覧
ghostty +list-actions
```

### 1.4 ランタイム設定リロード

```bash
# デフォルトキーバインド
# Linux: Ctrl+Shift+,
# macOS: Cmd+Shift+,

# カスタムキーバインドで設定
keybind = ctrl+shift+r=reload_config
```

> **注意**: 一部の設定はランタイムリロードに対応していません。新規ターミナル（ウィンドウ、タブ、スプリット）にのみ適用されるものがあります。

---

## 2. 設定ファイルの構成

### 2.1 モジュール分割（推奨構成）

```bash
# ~/.config/ghostty/config（メインファイル）
config-file = themes.conf
config-file = keybinds.conf
config-file = fonts.conf
config-file = ?local.conf  # ? プレフィックスで存在しなくてもエラーにならない
```

**ファイル構成例:**
```
~/.config/ghostty/
├── config          # メイン設定
├── themes.conf     # テーマ関連
├── keybinds.conf   # キーバインド
├── fonts.conf      # フォント設定
├── local.conf      # マシン固有設定（オプション）
└── themes/         # カスタムテーマ
    └── my-theme
```

### 2.2 設定ファイルの読み込み順序

1. デフォルト設定
2. テーマファイル（`theme` で指定）
3. メイン設定ファイル
4. `config-file` で指定した追加ファイル（定義順）

> **重要**: `config-file` は定義された設定ファイルの読み込み完了後に読み込まれます。後から読み込まれた設定が優先されます。

### 2.3 CLI引数との関係

```bash
# 全ての設定キーはCLI引数として使用可能
ghostty --font-size=14 --theme="Tokyo Night"

# CLIで指定した場合、設定ファイルの値をクリアして新しい値を設定
ghostty --font-family="Fira Code"
```

---

## 3. フォント設定

### 3.1 基本フォント設定

```bash
# メインフォント
font-family = JetBrains Mono

# スタイル別フォント（オプション）
font-family-bold = JetBrains Mono
font-family-italic = JetBrains Mono
font-family-bold-italic = JetBrains Mono

# フォントサイズ（ポイント）
font-size = 14

# 高DPIディスプレイでの奇数ピクセルサイズ
# 例: 1pt = 2px の場合、13.5pt で 27px
font-size = 13.5
```

### 3.2 フォールバックフォントの設定

```bash
# 複数指定でフォールバックチェーン構築
font-family = JetBrains Mono
font-family = Noto Sans CJK JP    # 日本語フォールバック
font-family = Noto Color Emoji    # 絵文字フォールバック

# リセットして再設定
font-family = ""
font-family = "My Favorite Font"
```

### 3.3 フォントスタイルの制御

```bash
# スタイル名で指定
font-style = Regular
font-style-bold = Heavy
font-style-italic = Italic

# スタイルを無効化
font-style-bold = false
font-style-italic = false
```

### 3.4 リガチャ・フォント機能

```bash
# リガチャを無効化
font-feature = -calt
font-feature = -liga
font-feature = -dlig

# 複数指定
font-feature = -calt, -liga, -dlig

# 特定機能を有効化
font-feature = +ss01
font-feature = cv01=2
```

> **注意**: Ghostty 1.2.0 からリガチャはデフォルトで無効になりました。

### 3.5 合成スタイル

```bash
# 合成スタイルの制御
font-synthetic-style = true         # 全て有効（デフォルト）
font-synthetic-style = false        # 全て無効
font-synthetic-style = no-bold      # ボールドのみ無効
font-synthetic-style = no-italic    # イタリックのみ無効
font-synthetic-style = no-bold,no-italic  # 複数無効化
```

### 3.6 フォント太字化（macOS）

```bash
# macOSでフォントを太く描画
font-thicken = true
font-thicken-strength = 127  # 0-255
```

### 3.7 セル・行間調整

```bash
# セルサイズ調整（値または%）
adjust-cell-width = 0
adjust-cell-height = 35%    # 行間を広げる

# ベースライン調整
adjust-font-baseline = 2

# 下線・取り消し線
adjust-underline-position = 0
adjust-underline-thickness = 1
adjust-strikethrough-position = 0
adjust-strikethrough-thickness = 1
```

---

## 4. テーマ・カラー設定

### 4.1 ビルトインテーマの使用

```bash
# 1.2.0以降: Title Case で指定
theme = Catppuccin Mocha
theme = Tokyo Night
theme = Dracula

# 1.2.0以前（非推奨）
theme = catppuccin-mocha

# テーマ一覧確認
ghostty +list-themes
ghostty +list-themes --color=dark   # ダークテーマのみ
ghostty +list-themes --color=light  # ライトテーマのみ
```

### 4.2 ライト/ダークモード自動切り替え

```bash
# システム設定に連動して自動切り替え
theme = light:Catppuccin Latte,dark:Catppuccin Mocha
theme = "light:Rose Pine Dawn,dark:Rose Pine"
```

### 4.3 カスタムカラー設定

```bash
# 基本色（テーマを上書き）
background = 1a1b26
foreground = c0caf5

# 選択色
selection-foreground = c0caf5
selection-background = 33467c
# 1.2.0以降の特別値
selection-foreground = cell-foreground
selection-background = cell-background

# カーソル色
cursor-color = c0caf5
cursor-text = 1a1b26
# 1.2.0以降の特別値
cursor-color = cell-foreground

# 256色パレット
palette = 0=#1a1b26   # 黒
palette = 1=#f7768e   # 赤
palette = 2=#9ece6a   # 緑
palette = 3=#e0af68   # 黄
palette = 4=#7aa2f7   # 青
palette = 5=#bb9af7   # マゼンタ
palette = 6=#7dcfff   # シアン
palette = 7=#a9b1d6   # 白
# 8-15: 明るいバージョン
palette = 8=#414868
# ...続く
```

### 4.4 カスタムテーマの作成

```bash
# ~/.config/ghostty/themes/my-theme
background = 1a1b26
foreground = c0caf5
cursor-color = f7768e
selection-background = 33467c

palette = 0=#1a1b26
palette = 1=#f7768e
# ...他のパレット色
```

```bash
# 使用方法
theme = my-theme
# または絶対パス
theme = /path/to/my-theme
```

### 4.5 コントラスト調整

```bash
# 最小コントラスト比（1-21）
# WCAG 2.0準拠のコントラスト比
minimum-contrast = 1.1   # 見えない文字を防ぐ
minimum-contrast = 3     # 読みやすさ重視
minimum-contrast = 4.5   # WCAG AA準拠
```

---

## 5. キーバインド設定

### 5.1 基本構文

```bash
# 構文: keybind = トリガー=アクション
keybind = ctrl+a=select_all
keybind = ctrl+shift+c=copy_to_clipboard
keybind = ctrl+shift+v=paste_from_clipboard
```

### 5.2 修飾キー

| 修飾キー | エイリアス |
|---------|-----------|
| `ctrl`  | `control` |
| `alt`   | `opt`, `option` |
| `super` | `cmd`, `command` |
| `shift` | - |

```bash
# 複数の修飾キー
keybind = ctrl+shift+t=new_tab
keybind = cmd+option+n=new_window
```

> **注意**: `fn`（globe）キーは修飾キーとして使用できません（OS/GUIツールキットの制限）。

### 5.3 物理キーとUnicodeキー

```bash
# Unicode（レイアウト依存）
keybind = ctrl+a=select_all

# 物理キー（レイアウト非依存、W3C仕様）
keybind = ctrl+KeyA=select_all

# スネークケースも可
keybind = ctrl+key_a=select_all
```

### 5.4 キーシーケンス（リーダーキー）

```bash
# > で区切ってシーケンス指定
keybind = ctrl+a>n=new_window
keybind = ctrl+a>t=new_tab
keybind = ctrl+a>s=new_split:right
keybind = ctrl+a>v=new_split:down

# リーダーキー自体を送信
keybind = ctrl+a>ctrl+a=text:\x01
```

> **注意**: シーケンスにはタイムアウトがありません。無期限に次のキーを待ちます。

### 5.5 プレフィックス修飾子

```bash
# all: 全ターミナル面に適用
keybind = all:ctrl+shift+q=quit

# global: システム全体（Ghostty非フォーカス時も有効）
keybind = global:cmd+grave_accent=toggle_quick_terminal

# unconsumed: アプリケーションにも入力を送信
keybind = unconsumed:ctrl+a=reload_config

# performable: アクション実行可能時のみ消費
keybind = performable:ctrl+c=copy_to_clipboard

# 複合使用
keybind = global:unconsumed:ctrl+grave=toggle_quick_terminal
```

### 5.6 主要アクション一覧

```bash
# ウィンドウ・タブ操作
keybind = ctrl+shift+n=new_window
keybind = ctrl+shift+t=new_tab
keybind = ctrl+shift+w=close_surface
keybind = ctrl+shift+q=quit

# スプリット操作
keybind = ctrl+shift+o=new_split:right
keybind = ctrl+shift+e=new_split:down
keybind = ctrl+shift+enter=new_split:auto

# ナビゲーション
keybind = ctrl+tab=next_tab
keybind = ctrl+shift+tab=previous_tab
keybind = alt+1=goto_tab:1
keybind = ctrl+shift+left=goto_split:left
keybind = ctrl+shift+right=goto_split:right

# スクロール
keybind = ctrl+shift+page_up=scroll_page_up
keybind = ctrl+shift+page_down=scroll_page_down
keybind = ctrl+shift+home=scroll_to_top
keybind = ctrl+shift+end=scroll_to_bottom

# プロンプト間ジャンプ（シェル統合必要）
keybind = ctrl+shift+up=jump_to_prompt:-1
keybind = ctrl+shift+down=jump_to_prompt:1

# クリップボード
keybind = ctrl+shift+c=copy_to_clipboard
keybind = ctrl+shift+v=paste_from_clipboard

# フォントサイズ
keybind = ctrl+plus=increase_font_size:1
keybind = ctrl+minus=decrease_font_size:1
keybind = ctrl+0=reset_font_size

# その他
keybind = ctrl+shift+comma=reload_config
keybind = ctrl+shift+i=inspector:toggle
keybind = f11=toggle_fullscreen
```

### 5.7 テキスト送信・エスケープシーケンス

```bash
# テキスト送信（Zig文字列リテラル構文）
keybind = ctrl+a=text:\x01        # Ctrl+A
keybind = alt+enter=text:\x1b\x0d # Alt+Enter

# CSIシーケンス
keybind = up=csi:A
keybind = down=csi:B

# ESCシーケンス
keybind = alt+d=esc:d  # 単語末まで削除
```

### 5.8 キーバインドの解除・クリア

```bash
# 特定のバインドを解除
keybind = ctrl+shift+t=unbind

# 全キーバインドをクリア（デフォルト含む）
keybind = clear

# その後カスタム定義
keybind = ctrl+t=new_tab
```

---

## 6. シェル統合

### 6.1 概要

シェル統合により以下の機能が有効になります：

- 作業ディレクトリの継承（新規タブ/スプリット）
- プロンプト間ジャンプ
- プロンプト表示時の確認なしクローズ
- 複雑なプロンプトのリサイズ改善
- コマンド出力の選択（Ctrl/Cmd+トリプルクリック）
- プロンプトでのカーソルバー化
- Alt/Optionクリックでカーソル移動

### 6.2 設定

```bash
# 自動検出（デフォルト）
shell-integration = detect

# シェル指定
shell-integration = zsh
shell-integration = bash
shell-integration = fish
shell-integration = elvish

# 無効化
shell-integration = none
```

### 6.3 機能の個別制御

```bash
# 全機能有効（デフォルト）
shell-integration-features = cursor,sudo,title

# 機能を無効化
shell-integration-features = no-cursor
shell-integration-features = no-sudo
shell-integration-features = no-title

# 複合指定
shell-integration-features = cursor,no-sudo,no-title

# SSH統合（1.2.0以降）
shell-integration-features = ssh-env,ssh-terminfo
```

| 機能 | 説明 |
|-----|------|
| `cursor` | プロンプトでブリンクバーカーソル |
| `sudo` | terminfoを保持するsudoラッパー |
| `title` | ウィンドウタイトル自動設定 |
| `ssh-env` | SSH時にTERMをxterm-256colorに変換 |
| `ssh-terminfo` | リモートホストにterminfoを自動インストール |

### 6.4 手動セットアップ

macOSのデフォルトbash（/bin/bash）など自動統合が効かない場合：

```bash
# .bashrc / .zshrc
if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
    source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"
fi
```

---

## 7. ウィンドウ・外観設定

### 7.1 基本ウィンドウ設定

```bash
# 初期サイズ（セル単位）
window-width = 120
window-height = 35

# 初期位置（ピクセル、macOSのみ）
window-position-x = 100
window-position-y = 100

# 起動状態
maximize = false
fullscreen = false
```

### 7.2 ウィンドウ装飾

```bash
# 装飾スタイル
window-decoration = auto     # OS/DEに従う（デフォルト）
window-decoration = none     # 装飾なし
window-decoration = client   # クライアントサイド
window-decoration = server   # サーバーサイド（Linux GTK）

# ブール値も受け付ける
window-decoration = true     # autoと同じ
window-decoration = false    # noneと同じ
```

### 7.3 パディング設定

```bash
# 水平パディング
window-padding-x = 8
window-padding-x = 8,16   # 左,右

# 垂直パディング
window-padding-y = 8
window-padding-y = 8,16   # 上,下

# 自動バランス
window-padding-balance = true

# パディング領域の色
window-padding-color = background   # 背景色
window-padding-color = extend       # 隣接セルの色を拡張
window-padding-color = extend-always
```

### 7.4 背景透過・ブラー

```bash
# 透明度（0.0-1.0）
background-opacity = 0.9

# セル背景にも適用（1.2.0以降）
background-opacity-cells = true

# 背景ブラー
background-blur = true
background-blur = 20    # ブラー強度（1-20）
```

### 7.5 背景画像（1.2.0以降）

```bash
background-image = /path/to/image.png
background-image-opacity = 0.8
background-image-position = center
background-image-fit = contain    # contain, cover, stretch, none
background-image-repeat = false
```

### 7.6 カーソル設定

```bash
# スタイル
cursor-style = block       # block, bar, underline, block_hollow
cursor-style-blink = true

# 色
cursor-color = f7768e
cursor-text = 1a1b26
cursor-opacity = 1.0

# サイズ調整
adjust-cursor-thickness = 2
adjust-cursor-height = 0

# クリックでカーソル移動
cursor-click-to-move = true
```

### 7.7 スプリット設定

```bash
# 非フォーカススプリットの透明度
unfocused-split-opacity = 0.8

# 非フォーカススプリットのオーバーレイ色
unfocused-split-fill = 1a1b26

# 分割線の色（1.1.0以降）
split-divider-color = 414868
```

### 7.8 タブバー設定（GTK）

```bash
# タブバー表示
window-show-tab-bar = auto     # 2タブ以上で表示
window-show-tab-bar = always   # 常に表示
window-show-tab-bar = never    # 非表示

# 新規タブ位置
window-new-tab-position = current  # 現在のタブの後
window-new-tab-position = end      # 末尾
```

### 7.9 リサイズオーバーレイ

```bash
# 表示タイミング
resize-overlay = after-first  # 初回以降（デフォルト）
resize-overlay = always
resize-overlay = never

# 位置
resize-overlay-position = center
resize-overlay-position = top-right

# 表示時間
resize-overlay-duration = 750ms
```

---

## 8. パフォーマンス最適化

### 8.1 GTKシングルインスタンス（Linux）

```bash
# 強く推奨：起動時間・メモリ使用量を大幅改善
gtk-single-instance = true
```

**効果:**
- 起動時間: 2-3秒 → 50-100ms
- メモリ使用量: 300MB → 150MB（追加ウィンドウあたり）

### 8.2 VSync設定

```bash
# レンダリング同期（macOSのみ）
window-vsync = true   # ティアリング防止（デフォルト）
window-vsync = false  # 入力遅延最小化
```

> **警告**: macOS 14.4以降では`false`でカーネルパニックの可能性があります。

### 8.3 スクロールバック制限

```bash
# デフォルト: 非常に大きい
# メモリ使用量を抑えるなら制限
scrollback-limit = 10000000  # 約10MB
scrollback-limit = 1000000   # 約1MB
```

### 8.4 cgroup制限（Linux）

```bash
linux-cgroup = single-instance
linux-cgroup-memory-limit = 2147483648    # 2GB
linux-cgroup-processes-limit = 1000
```

### 8.5 FreeType設定（Linux）

```bash
# ヒンティング設定
freetype-load-flags = hinting
freetype-load-flags = no-hinting
freetype-load-flags = force-autohint
freetype-load-flags = autohint
freetype-load-flags = monochrome  # 1-bitレンダリング
```

---

## 9. プラットフォーム固有設定

### 9.1 macOS設定

```bash
# タイトルバースタイル
macos-titlebar-style = native       # ネイティブ
macos-titlebar-style = transparent  # 透過（デフォルト）
macos-titlebar-style = tabs         # タブ統合
macos-titlebar-style = hidden       # 非表示

# ウィンドウボタン（1.2.0以降）
macos-window-buttons = visible
macos-window-buttons = hidden

# プロキシアイコン
macos-titlebar-proxy-icon = visible
macos-titlebar-proxy-icon = hidden

# Optionキーの動作
macos-option-as-alt = true   # Altとして扱う
macos-option-as-alt = false  # macOSネイティブ
macos-option-as-alt = left   # 左Optionのみ
macos-option-as-alt = right  # 右Optionのみ

# フルスクリーン
macos-non-native-fullscreen = false        # ネイティブ
macos-non-native-fullscreen = true         # 非ネイティブ
macos-non-native-fullscreen = visible-menu # メニューバー表示
macos-non-native-fullscreen = padded-notch # ノッチ回避

# ウィンドウシャドウ
macos-window-shadow = true

# ウィンドウ状態保存
window-save-state = default  # システム依存
window-save-state = always   # 常に保存
window-save-state = never    # 保存しない

# カスタムアイコン
macos-icon = official
macos-icon = blueprint
macos-icon = custom
macos-custom-icon = ~/.config/ghostty/Ghostty.icns

# Dock非表示（1.2.0以降）
macos-hidden = never
macos-hidden = always

# セキュア入力
macos-auto-secure-input = true
macos-secure-input-indication = true

# Shortcuts統合
macos-shortcuts = true
```

### 9.2 Linux/GTK設定

```bash
# ウィンドウテーマ
window-theme = auto
window-theme = system
window-theme = light
window-theme = dark
window-theme = ghostty  # Ghostty設定の色を使用

# タイトルバー色（window-theme = ghosttyの場合）
window-titlebar-background = 1a1b26
window-titlebar-foreground = c0caf5

# ウィンドウサブタイトル（1.1.0以降）
window-subtitle = false
window-subtitle = working-directory

# タイトルバーフォント（1.1.0以降）
window-title-font-family = "Noto Sans"

# アプリケーションクラス
class = com.mitchellh.ghostty

# X11インスタンス名
x11-instance-name = ghostty
```

---

## 10. 推奨設定テンプレート

### 10.1 ミニマル設定（初心者向け）

```bash
# ~/.config/ghostty/config
# ミニマル設定 - Ghosttyの優れたデフォルトを活かす

# テーマ
theme = Catppuccin Mocha

# フォント
font-size = 14

# ウィンドウ
window-padding-x = 8
window-padding-y = 8
```

### 10.2 開発者向け設定

```bash
# ~/.config/ghostty/config
# 開発者向け最適化設定

# テーマ（ライト/ダーク自動切り替え）
theme = "light:Catppuccin Latte,dark:Catppuccin Mocha"

# フォント
font-family = JetBrains Mono
font-family = Noto Sans CJK JP
font-size = 14
font-feature = -calt  # リガチャ無効

# カーソル
cursor-style = block
cursor-style-blink = false
shell-integration-features = cursor,sudo,title,ssh-env,ssh-terminfo

# ウィンドウ
window-padding-x = 12
window-padding-y = 12
window-padding-balance = true
background-opacity = 0.95

# パフォーマンス（Linux）
gtk-single-instance = true

# キーバインド（tmux風リーダーキー）
keybind = ctrl+a>c=new_tab
keybind = ctrl+a>x=close_surface
keybind = ctrl+a>v=new_split:right
keybind = ctrl+a>s=new_split:down
keybind = ctrl+a>h=goto_split:left
keybind = ctrl+a>j=goto_split:down
keybind = ctrl+a>k=goto_split:up
keybind = ctrl+a>l=goto_split:right
keybind = ctrl+a>1=goto_tab:1
keybind = ctrl+a>2=goto_tab:2
keybind = ctrl+a>3=goto_tab:3

# クイックターミナル
keybind = global:ctrl+grave_accent=toggle_quick_terminal
quick-terminal-position = top
quick-terminal-size = 40%
quick-terminal-animation-duration = 0.15
quick-terminal-autohide = true
```

### 10.3 Linux Wayland向け設定

```bash
# ~/.config/ghostty/config
# Linux Wayland 最適化設定

theme = "light:Catppuccin Latte,dark:Catppuccin Mocha"

font-family = JetBrains Mono
font-size = 12

# パフォーマンス最適化
gtk-single-instance = true

# ウィンドウ
window-decoration = auto
window-padding-x = 16
window-padding-y = 16
window-padding-balance = true
window-theme = ghostty
window-titlebar-background = 1e1e2e
window-titlebar-foreground = cdd6f4

# 背景
background-opacity = 0.9
background-blur = 20

# シェル統合
shell-integration = detect
shell-integration-features = cursor,sudo,title,ssh-env

# クイックターミナル
keybind = global:ctrl+grave_accent=toggle_quick_terminal
quick-terminal-position = top
gtk-quick-terminal-layer = top
quick-terminal-keyboard-interactivity = on-demand
```

### 10.4 macOS向け設定

```bash
# ~/.config/ghostty/config
# macOS 最適化設定

theme = "light:Catppuccin Latte,dark:Catppuccin Mocha"

font-family = SF Mono
font-family = Noto Sans CJK JP
font-size = 14
font-thicken = true

# macOS固有
macos-titlebar-style = transparent
macos-option-as-alt = true
macos-window-shadow = true
window-colorspace = display-p3

# ウィンドウ
window-padding-x = 12
window-padding-y = 8,12
window-padding-balance = true
background-opacity = 0.95
background-blur = 20

# カーソル
cursor-style = block
cursor-style-blink = true

# クイックターミナル
keybind = global:cmd+grave_accent=toggle_quick_terminal
quick-terminal-position = top
quick-terminal-size = 40%
quick-terminal-animation-duration = 0.2
quick-terminal-autohide = true
quick-terminal-space-behavior = move

# シェル統合
shell-integration-features = cursor,sudo,title,ssh-env,ssh-terminfo
```

---

## 11. トラブルシューティング

### 11.1 一般的な問題

**設定が反映されない**
```bash
# 設定検証
ghostty +validate-config

# ログ確認
ghostty 2>&1 | grep -i error
```

**フォントが見つからない**
```bash
# 利用可能なフォント確認
ghostty +list-fonts
ghostty +list-fonts --family="JetBrains"
```

**テーマが見つからない**
```bash
# 利用可能なテーマ確認
ghostty +list-themes

# 1.2.0以降: Title Case で指定
# NG: catppuccin-mocha
# OK: Catppuccin Mocha
```

### 11.2 シェル統合の問題

**自動検出が機能しない**
```bash
# ログで確認
# 正常: "shell integration automatically injected"
# 異常: 上記メッセージがない

# 手動設定
shell-integration = zsh

# 手動ソース（.zshrc）
if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
    source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"
fi
```

**macOSデフォルトbash**
macOSの`/bin/bash`は古く、自動統合非対応。Homebrewで新しいbashをインストールするか、手動ソースを使用。

### 11.3 キーバインドの問題

**キーバインドが効かない**
```bash
# 現在のキーバインド確認
ghostty +list-keybinds

# 物理キーで試す
keybind = ctrl+KeyA=select_all

# システムショートカットとの競合確認
# macOS: システム環境設定 > キーボード > ショートカット
```

**globalキーバインドが効かない（macOS）**
システム環境設定 > プライバシーとセキュリティ > アクセシビリティでGhosttyを許可。

**globalキーバインドが効かない（Linux）**
Global Shortcutsプロトコルに対応したDE/WMが必要：
- KDE Plasma 5.27以降
- GNOME 48以降
- Hyprland（要設定）
- Sway（未対応）

### 11.4 パフォーマンスの問題

**起動が遅い（Linux）**
```bash
# シングルインスタンスモード有効化
gtk-single-instance = true
```

**レンダリングが重い**
```bash
# カスタムシェーダー無効化
custom-shader-animation = false

# 背景ブラー無効化
background-blur = false

# スクロールバック削減
scrollback-limit = 1000000
```

### 11.5 便利なデバッグコマンド

```bash
# 設定の完全ダンプ
ghostty +show-config --default --docs > ghostty-config-full.txt

# アクション一覧
ghostty +list-actions

# クラッシュレポート
ghostty +crash-report

# バージョン確認
ghostty +version
```

---

## 参考リンク

- [公式ドキュメント](https://ghostty.org/docs)
- [設定リファレンス](https://ghostty.org/docs/config/reference)
- [キーバインドリファレンス](https://ghostty.org/docs/config/keybind)
- [GitHub リポジトリ](https://github.com/ghostty-org/ghostty)
- [Discord コミュニティ](https://discord.gg/ghostty)
- [iterm2-color-schemes（テーマソース）](https://iterm2colorschemes.com/)

---

*最終更新: 2026年1月 - Ghostty 1.2.0対応*
