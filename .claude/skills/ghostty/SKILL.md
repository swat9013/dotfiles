---
name: ghostty
description: Ghosttyターミナルの設定変更・トラブルシューティングを支援する知識ベーススキル。Use when「Ghostty」「ghostty設定」「ターミナル設定」「keybind」「テーマ変更」。
user-invocable: false
---

# Ghostty 知識ベース

## バージョン情報

現在のバージョン: `!`ghostty +version``

記録バージョン: `1.3.1`

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

---

## このdotfilesの現行Ghostty設定サマリ

| 項目 | 値 |
|------|----|
| 設定ファイル | `ghostty/config` → `~/.config/ghostty/config` |
| テーマ | TokyoNight Night |
| font-size | 12 |
| macos-titlebar-style | transparent |
| macos-option-as-alt | true |
| shell-integration | detect |
| shell-integration-features | cursor, sudo, title |
| window-save-state | always |
| desktop-notifications | true |
| window-padding-x/y | 8 |
| unfocused-split-opacity | 0.80 |
| unfocused-split-fill | #000000 |
| split-divider-color | #3d59a1（Tokyo Nightアクセント） |

---

## 設定確認コマンド早見表

```bash
ghostty +show-config          # 現在の設定表示
ghostty +validate-config      # 設定検証
ghostty +list-themes          # テーマ一覧
ghostty +list-fonts           # フォント一覧
ghostty +list-keybinds        # キーバインド一覧
ghostty +list-actions         # アクション一覧
```

---

## 主要設定カテゴリ概要

| カテゴリ | 代表設定 | 詳細 |
|---------|---------|------|
| テーマ・カラー | `theme`, `background-opacity` | references/config-and-keybind.md §3 |
| フォント | `font-family`, `font-size`, `font-feature` | references/config-and-keybind.md §4 |
| ウィンドウ・スプリット | `window-padding-*`, `unfocused-split-opacity` | references/config-and-keybind.md §5 |
| キーバインド | `keybind = key=action`、global/unconsumed修飾 | references/config-and-keybind.md §10 |
| シェル統合 | `shell-integration`, `shell-integration-features` | references/config-and-keybind.md §7 |
| macOS固有 | `macos-titlebar-style`, `macos-option-as-alt` | references/config-and-keybind.md §9 |

---

## トラブルシューティング要点

| 症状 | 原因・対処 |
|------|---------|
| 設定が反映されない | `+validate-config`でエラー確認。一部設定はランタイムリロード非対応→新規ウィンドウで確認 |
| テーマが見つからない | 1.2.0以降はTitle Case指定必須（例: `Catppuccin Mocha`、`catppuccin-mocha`はNG） |
| シェル統合が動作しない | `.zshrc`に手動source追加（`$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration`） |
| Homebrewアップグレードエラー | `brew uninstall --cask ghostty@tip --force` → `brew install --cask ghostty@tip` でforce reinstall |
| globalキーバインドが効かない | システム環境設定 > プライバシーとセキュリティ > アクセシビリティでGhosttyを許可 |
| Emacs相性問題 | references/emacs-compatibility.md参照 |

---

## references案内

| ファイル | 内容 |
|---------|------|
| `references/config-and-keybind.md` | 全設定項目・キーバインド構文・現行config一覧の詳細リファレンス |
| `references/applescript-automation.md` | AppleScript API・オブジェクトモデル・tmuxinator的自動化パターン |
| `references/emacs-compatibility.md` | Emacs + Ghostty相性不具合・keybind競合ガイド |
