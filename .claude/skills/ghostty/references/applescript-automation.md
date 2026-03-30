# Ghostty AppleScript自動化リファレンス

> 前提: `macos-applescript = true`（現行configで有効済み）
> 辞書: `/Applications/Ghostty.app/Contents/Resources/Ghostty.sdef`

## TOC

1. [オブジェクトモデル](#1-オブジェクトモデル)
2. [コマンド一覧](#2-コマンド一覧)
3. [surface configuration](#3-surface-configuration)
4. [tmuxinator的パターン](#4-tmuxinator的パターン)
5. [注意点](#5-注意点)

---

## 1. オブジェクトモデル

```
application
  └ window[]
      ├ .selected tab → tab
      └ tab[]
          ├ .index (1-based)
          ├ .focused terminal → terminal
          └ terminal[]
              └ .working directory
```

---

## 2. コマンド一覧

### ウィンドウ・タブ

| コマンド | 戻り値 |
|---------|--------|
| `new window [with configuration cfg]` | window |
| `new tab [in <window>] [with configuration cfg]` | tab |
| `activate window <window>` | - |
| `close window <window>` | - |
| `select tab <tab>` | - |
| `close tab <tab>` | - |

### ターミナル（スプリット）

| コマンド | 戻り値 |
|---------|--------|
| `split <terminal> direction (right\|left\|down\|up) [with configuration cfg]` | terminal |
| `focus <terminal>` | - |
| `close <terminal>` | - |

### 入力・操作

| コマンド | 用途 |
|---------|------|
| `input text "text\n" to <terminal>` | テキスト送信（`\n`でEnter） |
| `send key "enter" [action press\|release] [modifiers "shift,control,..."] to <terminal>` | キー送信 |
| `perform action "action-name" on <terminal>` | Ghosttyアクションを名前で実行 |

---

## 3. surface configuration

`new window`、`new tab`、`split` の `with configuration` に渡せるプロパティ:

| プロパティ | 型 | 用途 |
|-----------|-----|------|
| `initial working directory` | text | 作業ディレクトリ |
| `command` | text | シェルの代わりに実行するコマンド |
| `initial input` | text | シェル起動後に送信するテキスト |
| `font size` | real | フォントサイズ |
| `wait after command` | boolean | コマンド終了後もペインを保持 |
| `environment variables` | list of text | `{"KEY=value", ...}` |

---

## 4. tmuxinator的パターン

```applescript
tell application "Ghostty"
    set cfg to {initial working directory:"/path/to/project"}
    set w to new window with configuration cfg
    set editor to focused terminal of selected tab of w
    set server to split editor direction right with configuration cfg
    input text "nvim .\n" to editor
    input text "npm run dev\n" to server
end tell
```

`osascript`でシェルから呼べるため、YAML定義→AppleScript生成のラッパーで実現可能。

---

## 5. 注意点

- **macOS専用**: Linux/GTKはD-Bus経由（`ghostty +new-window`が使える）
- **`input text`はペースト動作**: シェルプロンプト待ちなし。確実にプロンプト後に実行するなら`initial input`を使うか`delay`を挟む
- **IPC不在**: macOSではAppleScriptが唯一のプロセス間制御手段
