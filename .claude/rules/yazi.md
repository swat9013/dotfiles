---
paths:
  - yazi/**
  - .zsh/yazi.zsh
---

# Yazi設定ガイド

## コマンド

| コマンド | 機能 |
|---------|------|
| `y` | cd on quit付きで起動 |
| `yg` | ghqルートディレクトリで起動 |
| `ygh` | fzyでリポジトリ選択 → yazi起動 |

## Version 26の破壊的変更

yaziのv26では設定構文が大幅に変更:

| Before | After |
|--------|-------|
| `[manager]` | `[mgr]` |
| `keymap` | `prepend_keymap`（デフォルトより優先） |

### キー表記

- **単一キー**: 文字列形式 `"<C-p>"`
- **複数キーシーケンス**: 配列形式 `["<C-x>", "<C-c>"]`

**重要**: 設定変更後は必ず`yazi --clear-cache`を実行。

詳細: `docs/yazi-configuration-guide.md`

## ghosttyとのキーバインド共存

| キーバインド | 処理レベル | 機能 |
|------------|-----------|------|
| Ctrl+Shift+n/p/b/f | ghostty（ターミナル） | スプリット移動 |
| Ctrl+n/p/f/b | yazi（アプリケーション） | カーソル移動 |

処理レベルが異なるため、両方が自然に共存。

## ファイル管理の棲み分け

| ツール | 用途 | 起動 |
|-------|------|------|
| fzy | コマンド履歴検索 | Ctrl+r |
| fzy | ディレクトリジャンプ | Ctrl+s |
| fzy | ghqリポジトリ選択 | `gh` |
| yazi | ファイルブラウジング・操作 | `y` |

## 設定ファイル

- `yazi/yazi.toml` - 全般設定
- `yazi/keymap.toml` - キーバインド
- `yazi/theme.toml` - テーマ（Tokyo Night）
