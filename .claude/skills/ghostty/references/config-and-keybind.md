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
8-9. [通知・macOS固有](#8-9-通知macos固有) → `ghostty-advanced.md`
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

コメントアウト中（Zellij連携候補）: `cmd+t=unbind`, `ctrl+tab=csi:9;5u`, `focus-follows-mouse`

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

| キー | 説明 |
|------|------|
| `font-family` | フォントファミリー（複数行でフォールバック） |
| `font-size` | フォントサイズ（pt）★ 現行: `12` |
| `font-feature` | OpenType機能（例: `-calt` でリガチャ無効） |
| `font-thicken` | ストロークを太く（macOS限定） |

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

| キー | 説明 |
|------|------|
| `background` / `foreground` | 背景色/前景色（hex/X11） |
| `background-opacity` | 背景透明度（macOS 26.0+: `macos-glass-regular`/`macos-glass-clear` も可） |
| `desktop-notifications` | デスクトップ通知（OSC 9/777対応）★ 現行: `true` |

---

## 6. カーソル

`cursor-style`（block/bar/underline/block_hollow）、`cursor-style-blink`、`cursor-color`、`cursor-click-to-move`（v1.3.0+、シェル統合必須）

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

- 作業ディレクトリ継承、プロンプト間ジャンプ、Option/Altクリック移動、コマンド出力選択

手動設定（自動注入失敗時）: `.zshrc` で `source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"` を `$GHOSTTY_RESOURCES_DIR` 存在時に実行

---

## 8-9. 通知・macOS固有

詳細は `ghostty-advanced.md` を参照。

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

`global:`（システム全体）、`all:`（非フォーカス含む）、`unconsumed:`（プログラムにも送信）、`performable:`（実行可能時のみ）

### キーシーケンス

`>` で連鎖: `keybind = ctrl+a>n=new_tab`（tmux/Emacsプレフィックスキー相当）

### アンバインド・転送

`unbind`（解除）、`csi:9;5u`（CSI転送）、`text:foo`（テキスト送信）、`esc:foo`（Esc送信）

Key Tables・アクション一覧は `ghostty-advanced.md` を参照。

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
