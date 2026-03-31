# Zed 設定・キーマップリファレンス

> **凡例**: ★ = 現行dotfiles使用中

## TOC

- [§1 エディタ基本設定](#1-エディタ基本設定)
- [§2 テーマ設定](#2-テーマ設定)
- [§3 自動保存・フォーマット設定](#3-自動保存フォーマット設定)
- [§4 Terminal設定](#4-terminal設定)
- [§5 Git設定](#5-git設定)
- [§6 Vim設定](#6-vim設定)
- [§7 AI/Assistant設定](#7-aiassistant設定)
- [§8 UI設定](#8-ui設定)
- [§9 keymap.json構文](#9-keymapjson構文)
- [§10 tasks.json構文と変数](#10-tasksjson構文と変数)

---

## §1 エディタ基本設定

| キー | 型 | デフォルト | 説明 |
|------|----|-----------|------|
| `buffer_font_family` | string | システム依存 | エディタフォント ★ `"PlemolJP35"` |
| `buffer_font_size` | number | 15 | フォントサイズ（pt）★ `12` |
| `tab_size` | number | 4 | タブ幅 ★ `2` |
| `hard_tabs` | boolean | false | タブ文字使用 ★ `false` |
| `ensure_final_newline_on_save` | boolean | true | 末尾改行保証 ★ `true` |
| `remove_trailing_whitespace_on_save` | boolean | true | 末尾空白削除 ★ `true` |
| `cursor_blink` | boolean | true | カーソル点滅 ★ `false` |
| `restore_on_startup` | string | `"last_session"` | 起動時セッション復元 ★ `"last_session"` |

---

## §2 テーマ設定

| キー | 型 | デフォルト | 説明 |
|------|----|-----------|------|
| `theme.mode` | string | `"system"` | 切替モード ★ `"system"` |
| `theme.dark` | string | `"One Dark"` | ダークテーマ ★ `"Tokyo Night"` |
| `theme.light` | string | `"One Light"` | ライトテーマ ★ `"Tokyo Night Light"` |

- `theme` はグローバル設定のみ有効（プロジェクト設定で上書き不可）
- テーマ一覧: コマンドパレット `theme selector: toggle`

---

## §3 自動保存・フォーマット設定

| キー | 型 | デフォルト | 説明 |
|------|----|-----------|------|
| `autosave` | string/object | `"off"` | 自動保存 ★ `"on_focus_change"` |
| `format_on_save` | string | `"on"` | 保存時フォーマット ★ `"off"` |
| `formatter` | object | - | フォーマッター指定 |

**autosave 選択肢**: `"off"` / `"on_focus_change"` ★ / `"on_window_change"` / `{ "after_delay": { "milliseconds": N } }`

---

## §4 Terminal設定

| キー | 型 | デフォルト | 説明 |
|------|----|-----------|------|
| `terminal.shell.program` | string | システムシェル | ターミナルシェル |
| `terminal.font_family` | string | `buffer_font_family` | ターミナルフォント |
| `terminal.font_size` | number | `buffer_font_size` | フォントサイズ |
| `terminal.working_directory` | string | `"current_project_directory"` | 起動ディレクトリ |
| `terminal.env` | object | - | 環境変数 |

**working_directory 選択肢**: `"current_project_directory"` / `"current_file_directory"` / `"always_home"` / `{ "always": "/path" }`

---

## §5 Git設定

| キー | 型 | デフォルト | 説明 |
|------|----|-----------|------|
| `git.git_gutter` | string | `"tracked_files"` | ガター差分表示（`"tracked_files"` / `"hide"`） |
| `git_panel.tree_view` | boolean | false | Gitパネルをツリー表示 ★ `true` |

---

## §6 Vim設定

| キー | 型 | デフォルト | 説明 |
|------|----|-----------|------|
| `vim_mode` | boolean | false | Vimモード有効化 |
| `vim.default_mode` | string | `"normal"` | デフォルトモード |
| `vim.use_smartcase_find` | boolean | false | スマートケース検索 |
| `vim.use_system_clipboard` | string | `"never"` | システムクリップボード（`"never"` / `"always"` / `"on_yank"`） |

- `vim_mode` はグローバル設定のみ有効

---

## §7 AI/Assistant設定

| キー | 型 | デフォルト | 説明 |
|------|----|-----------|------|
| `assistant.enabled` | boolean | true | AIアシスタント有効化 |
| `assistant.default_model` | object | - | デフォルトモデル |
| `assistant.version` | string | `"2"` | UIバージョン |

```json
"assistant": { "default_model": { "provider": "anthropic", "model": "claude-opus-4-5" }, "version": "2" }
```

---

## §8 UI設定

| キー | 型 | デフォルト | 説明 |
|------|----|-----------|------|
| `ui_font_size` | number | 16 | UIフォントサイズ |
| `relative_line_numbers` | boolean | false | 相対行番号 |
| `show_whitespace` | string | `"none"` | 空白文字表示（`"none"` / `"all"` / `"boundary"` / `"trailing"`） |
| `telemetry.diagnostics` | boolean | true | 診断テレメトリ ★ `false` |
| `telemetry.metrics` | boolean | true | メトリクステレメトリ ★ `false` |

---

## §9 keymap.json構文

```json
[
  { "bindings": { "cmd-shift-c": "editor::CopyRelativePath" } },
  { "context": "vim_mode == normal", "bindings": { "g d": "editor::GoToDefinition" } }
]
```

現行バインド: `cmd-shift-c` → `editor::CopyRelativePath` ★

| 項目 | 説明 |
|------|------|
| コンテキスト例 | `"Editor"` / `"Editor && !VimControl"` / `"Terminal"` / `"ProjectPanel"` |
| Vim条件 | `"vim_mode == normal"` / `"vim_mode == insert"` |
| ブール演算 | `&&` (AND) / `\|\|` (OR) / `!` (NOT) |
| キーシーケンス | スペース区切り（例: `"g d"`） |
| アクション無効化 | 値を `null` に設定 |

---

## §10 tasks.json構文と変数

```json
[{ "label": "Run tests", "command": "cargo", "args": ["test"], "cwd": "$ZED_WORKTREE_ROOT", "reveal": "always", "save": "all" }]
```

**フィールド**: `label`（必須）/ `command`（必須）/ `args` / `cwd` / `env` / `reveal`（`"always"` / `"on_error"` / `"never"`）/ `save`（`"none"` / `"current"` / `"all"`）

| 変数 | 説明 |
|-----|------|
| `$ZED_FILE` | 現在のファイルの絶対パス |
| `$ZED_FILENAME` | ファイル名（拡張子付き） |
| `$ZED_SELECTED_TEXT` | 選択テキスト |
| `$ZED_WORKTREE_ROOT` | プロジェクトルート |
| `$ZED_LANGUAGE` | バッファの言語識別子 |
| `$ZED_ROW` / `$ZED_COLUMN` | カーソル位置（1始まり） |
| `$ZED_SYMBOL` | カーソル位置のシンボル名 |

**設定優先順位**: デフォルト < グローバル (`~/.config/zed/`) < プロジェクト (`.zed/`)
オブジェクト設定（`terminal`, `lsp` 等）はプロパティマージ。`theme`, `vim_mode` はグローバルのみ適用。
