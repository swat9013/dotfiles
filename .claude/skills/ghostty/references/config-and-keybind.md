# Ghostty 設定・キーバインドリファレンス

> **凡例**: ★ = 現行config使用中

## TOC

1. [設定ファイル基本](#1-設定ファイル基本)
2. [現行config一覧](#2-現行config一覧)
3. [テーマ](#3-テーマ)
4. [フォント](#4-フォント)
5. [ウィンドウ・外観](#5-ウィンドウ外観)
6. [カーソル](#6-カーソル)
7. [シェル統合](#7-シェル統合)
8. [通知](#8-通知)
9. [macOS固有](#9-macos固有)
10. [キーバインド](#10-キーバインド)
11. [設定リロード・検証](#11-設定リロード検証)

---

## 1. 設定ファイル基本

| 項目 | 値 |
|------|----|
| dotfiles内パス | `ghostty/config` |
| 実際のパス | `~/.config/ghostty/config` |
| 構文 | `key = value`（コメントは `#` 行頭のみ） |
| ホットリロード | `Cmd+Shift+,`（macOS）/ `Ctrl+Shift+,`（Linux） |
| 空値リセット | `font-family =` でデフォルトに戻す |

### モジュール分割

```
config-file = keybinds.conf
config-file = ?local.conf    # ? プレフィックス: ファイルなしでもエラーなし
```

---

## 2. 現行config一覧

```
theme = TokyoNight Night              # ★
font-size = 12                        # ★
macos-titlebar-style = transparent    # ★
macos-option-as-alt = true            # ★
window-padding-x = 8                  # ★
window-padding-y = 8                  # ★
unfocused-split-opacity = 0.80        # ★
unfocused-split-fill = 000000         # ★
shell-integration = detect            # ★
shell-integration-features = cursor,sudo,title  # ★
window-save-state = always            # ★
desktop-notifications = true          # ★
split-divider-color = #3d59a1         # ★（Tokyo Night アクセントカラー）
```

### コメントアウト中（Zellij連携候補）

```
# keybind = cmd+t=unbind
# keybind = ctrl+tab=csi:9;5u        # CSIシーケンス転送
# focus-follows-mouse = true
```

---

## 3. テーマ

| 構文 | 例 |
|------|----|
| 単一テーマ | `theme = TokyoNight Night` ★ |
| ライト/ダーク自動切替 | `theme = light:Tokyo Night Light,dark:Tokyo Night Storm` |

- テーマ一覧確認: `ghostty +list-themes`
- テーマ名は **Title Case**（`catppuccin-mocha` → NG、`Catppuccin Mocha` → OK）
- カスタムテーマ配置先: `~/.config/ghostty/themes/`
- テーマギャラリー: [ghostty.style](https://ghostty.style)（460+テーマ）

---

## 4. フォント

| キー | デフォルト | 説明 |
|------|-----------|------|
| `font-family` | システム依存 | フォントファミリー（複数指定でフォールバック） |
| `font-size` | システム依存 | フォントサイズ（pt）★ 現行: `12` |
| `font-feature` | - | OpenType機能（例: `-calt` でリガチャ無効） |
| `font-thicken` | false | ストロークを太く（macOS限定） |
| `font-variation` | - | 可変フォント軸（例: `wght=700`） |
| `font-codepoint-map` | - | Unicode範囲→フォントマッピング |

```
font-family = JetBrains Mono
font-family = Noto Sans CJK JP    # フォールバック（2行目以降）
font-size = 12
font-feature = -calt              # リガチャ無効化
```

---

## 5. ウィンドウ・外観

### ウィンドウレイアウト

| キー | 型 | 説明 |
|------|----|------|
| `window-padding-x` | points | 水平パディング（`left,right` 形式可）★ 現行: `8` |
| `window-padding-y` | points | 垂直パディング（`top,bottom` 形式可）★ 現行: `8` |
| `window-padding-balance` | boolean | 余分パディングをバランス |
| `window-save-state` | default\|never\|always | ウィンドウ状態保存（macOS限定）★ 現行: `always` |
| `window-decoration` | auto\|client\|server\|none | ウィンドウ装飾 |

### スプリット

| キー | 型 | 説明 |
|------|----|------|
| `unfocused-split-opacity` | 0-1 | 非フォーカス時スプリット不透明度 ★ 現行: `0.80` |
| `unfocused-split-fill` | hex | 非フォーカス時の塗り色 ★ 現行: `000000` |
| `split-divider-color` | color | 分割線の色 ★ 現行: `#3d59a1`（Tokyo Night アクセント） |

### 外観・カラー

| キー | 型 | 説明 |
|------|----|------|
| `background` | hex/X11 | 背景色 |
| `foreground` | hex/X11 | 前景色 |
| `background-opacity` | 0-1 | 背景透明度（macOS 26.0+は `macos-glass-regular`/`macos-glass-clear` も可） |
| `background-blur` | true\|number | 背景ぼかし（`true`=強度20） |
| `minimum-contrast` | number | 前景/背景の最小コントラスト比 |
| `desktop-notifications` | boolean | デスクトップ通知（OSC 9/777対応）★ 現行: `true` |

---

## 6. カーソル

| キー | デフォルト | 説明 |
|------|-----------|------|
| `cursor-style` | block | 形状: `block`\|`bar`\|`underline`\|`block_hollow` |
| `cursor-style-blink` | true | 点滅 |
| `cursor-color` | - | カーソル色（hex/X11/`cell-foreground`/`cell-background`） |
| `cursor-click-to-move` | - | プロンプト内クリック移動（シェル統合必須、v1.3.0+） |

---

## 7. シェル統合

### 現行設定

```
shell-integration = detect                       # ★ 自動検出
shell-integration-features = cursor,sudo,title   # ★
```

### features 全オプション

| 値 | 説明 |
|----|------|
| `cursor` | シェルモードによるカーソル形状変更 |
| `sudo` | sudo コマンド自動ラッピング |
| `title` | コマンドによるウィンドウタイトル更新 |
| `ssh-env` | SSH先にターミナル環境変数を転送 |
| `ssh-terminfo` | SSH先に terminfo を自動インストール |

### 有効になる機能

- 新規タブ/スプリット/ウィンドウで作業ディレクトリ継承
- プロンプト間ジャンプ（`jump_to_prompt` アクション）
- Option/Altクリックでカーソル移動
- コマンド出力選択（Ctrl/Cmd + triple-click）

### 手動設定（自動注入が失敗する場合）

```zsh
# .zshrc に追加
if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
    source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"
fi
```

---

## 8. 通知

v1.3.0+ 追加。

| キー | 型 | 説明 |
|------|----|------|
| `notify-on-command-finish` | never\|unfocused\|always | コマンド完了通知 |
| `notify-on-command-finish-action` | bell\|notify | 通知アクション |
| `notify-on-command-finish-after` | milliseconds | 最小実行時間（これ以上かかったら通知） |

---

## 9. macOS固有

| キー | 現行値 | 選択肢・説明 |
|------|--------|-------------|
| `macos-titlebar-style` | `transparent` ★ | `transparent`\|`tabs`\|`hidden`\|`native` |
| `macos-option-as-alt` | `true` ★ | `true`\|`false`\|`left`\|`right` |
| `macos-non-native-fullscreen` | - | 非ネイティブフルスクリーン |
| `macos-window-buttons` | - | ウィンドウボタン表示制御 |
| `background-opacity` | - | `macos-glass-regular`/`macos-glass-clear`（macOS 26.0+） |

### クイックターミナル

```
keybind = global:cmd+grave_accent=toggle_quick_terminal
quick-terminal-position = top
quick-terminal-size = 40%
quick-terminal-autohide = true
```

> `global:` プレフィックスにはアクセシビリティ権限が必要（システム環境設定 > プライバシーとセキュリティ > アクセシビリティ）

---

## 10. キーバインド

### 基本構文

```
keybind = trigger=action
keybind = ctrl+shift+t=new_tab
```

### 修飾キー

| 修飾キー | 別名 |
|---------|------|
| `ctrl` | `control` |
| `alt` | `opt`, `option` |
| `shift` | - |
| `super` | `cmd`, `command` |

### 特殊プレフィックス

| プレフィックス | 説明 |
|--------------|------|
| `global:` | システム全体で機能（macOS、アクセシビリティ権限必須） |
| `all:` | フォーカス外のサーフェスにも適用 |
| `unconsumed:` | 入力を消費せずプログラムにも送信 |
| `performable:` | アクション実行可能な場合のみ消費 |

### キーシーケンス（リーダーキー）

```
keybind = ctrl+a>n=new_tab        # ctrl+a を押してから n
keybind = ctrl+a>t=new_window
keybind = ctrl+a>s=new_split:right
```

`>` でキーを連鎖（tmux/Emacs のプレフィックスキーと同様）。

### Key Tables（モーダル入力）v1.3.0+

```
keybind = ctrl+a=activate_key_table:my_table
keybind = my_table/n=new_tab
keybind = my_table/s=new_split:right
keybind = my_table/escape=reload_config   # テーブルを抜ける
```

`activate_key_table` で名前付きキーバインドセットを有効化。一回限りの起動か持続かは `--one-shot` / `--sticky` オプションで制御。

### アンバインド・転送

```
keybind = cmd+t=unbind                  # Ghosttyのデフォルトを解除
keybind = ctrl+tab=csi:9;5u            # CSIシーケンスとして転送（Zellij等に渡す）
keybind = ctrl+shift+b=text:foo        # テキスト送信
keybind = ctrl+shift+f=esc:foo         # Escシーケンス送信
```

### 主要アクション

**タブ・ウィンドウ**

| アクション | 説明 |
|-----------|------|
| `new_tab` | 新規タブ |
| `previous_tab` / `next_tab` | タブ切替 |
| `goto_tab:N` | N番目のタブへ |
| `close_tab` | タブ閉じる |
| `new_window` | 新規ウィンドウ |
| `toggle_fullscreen` | フルスクリーン |
| `toggle_quick_terminal` | クイックターミナル |
| `toggle_command_palette` | コマンドパレット |

**スプリット**

| アクション | 説明 |
|-----------|------|
| `new_split:right` / `new_split:down` | スプリット作成 |
| `goto_split:left/right/up/down/previous/next` | スプリット移動 |
| `toggle_split_zoom` | スプリットズーム |
| `resize_split:right,20` | スプリットリサイズ（px） |
| `equalize_splits` | スプリット均等化 |

**スクロール**

| アクション | 説明 |
|-----------|------|
| `scroll_page_up` / `scroll_page_down` | ページスクロール |
| `scroll_to_top` / `scroll_to_bottom` | 先頭/末尾へ |
| `scroll_page_lines:N` | N行スクロール |
| `jump_to_prompt:N` | プロンプト間移動（N: -1=上、1=下） |

**検索（v1.3.0+）**

| アクション | 説明 |
|-----------|------|
| `search` | スクロールバック検索開始 |
| `search_selection` | 選択テキストで検索 |

**その他**

| アクション | 説明 |
|-----------|------|
| `reload_config` | 設定リロード |
| `open_config` | 設定ファイルを開く |
| `increase_font_size:N` / `decrease_font_size:N` | フォントサイズ変更 |
| `reset_font_size` | フォントサイズリセット |
| `copy_to_clipboard` / `paste_from_clipboard` | クリップボード |
| `ignore` | キー入力を無視 |
| `unbind` | バインド解除 |

### Emacs風スプリット移動（設定候補）

```
keybind = ctrl+shift+b=goto_split:left     # backward
keybind = ctrl+shift+f=goto_split:right    # forward
keybind = ctrl+shift+p=goto_split:up       # previous
keybind = ctrl+shift+n=goto_split:down     # next
```

> 注意: `ctrl+shift+f` は v1.3.0+ のスクロールバック検索のデフォルトキーと競合する可能性がある。

---

## 11. 設定リロード・検証

```bash
ghostty +show-config                    # 現在の有効な設定を表示
ghostty +show-config --default --docs   # デフォルト値とドキュメント付き
ghostty +validate-config                # 設定ファイルの検証
ghostty +list-keybinds                  # 有効なキーバインド一覧
ghostty +list-actions                   # 利用可能なアクション一覧
ghostty +list-themes                    # テーマ一覧
ghostty +list-fonts                     # インストール済みフォント一覧
```
