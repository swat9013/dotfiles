# Ghostty アドバンスト設定

`config-and-keybind.md` から分離。

---

## 通知

v1.3.0+ 追加。

| キー | 型 | 説明 |
|------|----|------|
| `notify-on-command-finish` | never\|unfocused\|always | コマンド完了通知 |
| `notify-on-command-finish-action` | bell\|notify | 通知アクション |
| `notify-on-command-finish-after` | milliseconds | 最小実行時間（これ以上かかったら通知） |

---

## macOS固有

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

## Key Tables（モーダル入力）v1.3.0+

```
keybind = ctrl+a=activate_key_table:my_table
keybind = my_table/n=new_tab
keybind = my_table/s=new_split:right
keybind = my_table/escape=reload_config   # テーブルを抜ける
```

`activate_key_table` で名前付きキーバインドセットを有効化。一回限りの起動か持続かは `--one-shot` / `--sticky` オプションで制御。

---

## キーバインド アクション一覧

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
