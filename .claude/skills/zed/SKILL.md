---
name: zed
description: Zed editorの設定変更・拡張機能管理・トラブルシューティングを支援する知識ベーススキル。Use when「Zed」「zed設定」「keymap」「LSP」「拡張機能」。
user-invocable: false
---

# Zed 知識ベース

## バージョン情報

現在のバージョン: `!`zed --version``

記録バージョン: `0.229.0`

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

---

## このdotfilesの現行Zed設定サマリ

| 項目 | 値 |
|------|----|
| settings.json | `zed/settings.json` → `~/.config/zed/settings.json` |
| keymap.json | `zed/keymap.json` → `~/.config/zed/keymap.json` |
| テーマ | Tokyo Night (dark) / Tokyo Night Light (light)、system連動 |
| フォント | PlemolJP35 size 12 |
| tab_size | 2（hard_tabs: false） |
| 自動保存 | on_focus_change |
| format_on_save | off |
| telemetry | 無効 |
| カスタムkeybind | cmd-shift-c → CopyRelativePath |

---

## CLI早見表

```bash
zed myfile.txt              # ファイルを開く
zed myfile.txt:42           # 指定行で開く
zed ~/projects/myproject    # ディレクトリをワークスペースで開く
zed --new myfile.txt        # 新しいウィンドウで開く
zed --add myfile.txt        # 既存ウィンドウに追加
zed --diff file1 file2      # diff表示
zed --wait myfile.txt       # エディタ閉じるまで待機（EDITOR用）
zed -                       # 標準入力から開く
```

---

## 主要設定カテゴリ概要

優先順位: デフォルト < グローバル(`~/.config/zed/`) < プロジェクト(`.zed/`)。オブジェクト設定はマージ、theme等はグローバルのみ適用。

| カテゴリ | 代表設定 | 詳細 |
|---------|---------|------|
| エディタ基本 | `buffer_font_family`, `tab_size`, `hard_tabs` | references/settings-and-keymap.md §1 |
| テーマ | `theme.mode`, `theme.dark`, `theme.light` | references/settings-and-keymap.md §2 |
| 自動保存・フォーマット | `autosave`, `format_on_save`, `formatter` | references/settings-and-keymap.md §3 |
| LSP | `lsp.{server}.initialization_options` | references/extensions-and-lsp.md §2 |
| 言語固有 | `languages.{Lang}.tab_size`, `formatter` | references/extensions-and-lsp.md §3 |
| Terminal | `terminal.shell.program`, `terminal.font_family` | references/settings-and-keymap.md §4 |
| Git | `git.git_gutter`, `git_panel.tree_view` | references/settings-and-keymap.md §5 |
| Vim | `vim_mode`, `vim.use_smartcase_find` | references/settings-and-keymap.md §6 |
| AI/Assistant | `assistant.enabled`, `assistant.default_model` | references/settings-and-keymap.md §7 |
| UI | `ui_font_size`, `relative_line_numbers` | references/settings-and-keymap.md §8 |
| 拡張機能 | `auto_install_extensions` | references/extensions-and-lsp.md §1 |
| キーバインド | コンテキスト指定、キーシーケンス | references/settings-and-keymap.md §9 |
| タスク | `tasks.json`, `$ZED_FILE`等の変数 | references/settings-and-keymap.md §10 |

---

## トラブルシューティング要点

| 症状 | 原因・対処 |
|------|---------|
| LSPが動かない | コマンドパレット → `editor: restart language server` |
| 起動しない/クラッシュ | `~/Library/Application Support/Zed/db/` を一時移動して再起動 |
| フォント表示されない | `buffer_font_family` のフォント名正確性を確認 |
| キーバインドが効かない | `keymap.json` のコンテキスト条件を確認 |
| パフォーマンス低下 | LSPサーバー再起動、大規模プロジェクトのファイル除外設定 |
| テーマ適用されない | 拡張機能のインストール確認、`theme` 設定値を検証 |

デバッグコマンド（コマンドパレット）:
- `zed: open log` — アプリケーションログ表示
- `debug: open language server logs` — LSPサーバーログ
- `editor: restart language server` — LSP再起動
- `zed: copy system specs into clipboard` — バージョン・OS情報コピー

ログファイル: `~/Library/Logs/Zed/Zed.log`（macOS）

---

## references案内

| ファイル | 内容 |
|---------|------|
| `references/settings-and-keymap.md` | settings.json全カテゴリ詳細・keymap構文・tasks.json変数リファレンス |
| `references/extensions-and-lsp.md` | 拡張機能の宣言的管理・LSP設定・言語サーバー構成 |
