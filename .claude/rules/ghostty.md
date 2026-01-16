---
paths: ghostty/**
---

# Ghostty設定ガイド

## 設定確認コマンド

```bash
ghostty +show-config          # 現在の設定表示
ghostty +show-config --default --docs  # デフォルト設定（ドキュメント付き）
ghostty +validate-config      # 設定検証
ghostty +list-themes          # テーマ一覧
ghostty +list-fonts           # フォント一覧
ghostty +list-keybinds        # キーバインド一覧
ghostty +list-actions         # アクション一覧
```

## 設定ファイル

### 場所

- macOS: `~/.config/ghostty/config`
- dotfilesでは `ghostty/config` → `~/.config/ghostty/config` にシンボリックリンク

### 基本構文

```bash
key = value        # 基本形式
# コメント         # 行頭のみ有効
font-family =      # 空値でデフォルトにリセット
```

### モジュール分割

```bash
config-file = themes.conf
config-file = keybinds.conf
config-file = ?local.conf    # ? で存在しなくてもエラーなし
```

## プロジェクト固有設定

### テーマ設定

**Tokyo Night** テーマ（ライト/ダーク自動切り替え）

- **ライトモード**: Tokyo Night Light
- **ダークモード**: Tokyo Night Storm

```bash
theme = "light:Tokyo Night Light,dark:Tokyo Night Storm"
```

### Emacs風スプリット移動

| キーバインド | 機能 |
|------------|------|
| Ctrl+Shift+b | 左のスプリットへ移動 (backward) |
| Ctrl+Shift+f | 右のスプリットへ移動 (forward) |
| Ctrl+Shift+p | 上のスプリットへ移動 (previous) |
| Ctrl+Shift+n | 下のスプリットへ移動 (next) |

## 主要設定カテゴリ

### フォント

```bash
font-family = JetBrains Mono
font-family = Noto Sans CJK JP    # フォールバック（複数指定可）
font-size = 14
font-feature = -calt              # リガチャ無効化
```

### キーバインド

```bash
# 基本形式
keybind = ctrl+shift+t=new_tab

# リーダーキー（シーケンス）
keybind = ctrl+a>n=new_window
keybind = ctrl+a>t=new_tab

# 修飾プレフィックス
keybind = global:ctrl+grave_accent=toggle_quick_terminal  # システム全体
keybind = unconsumed:ctrl+a=reload_config                 # アプリにも送信
```

### シェル統合

```bash
shell-integration = detect        # 自動検出（デフォルト）
shell-integration-features = cursor,sudo,title,ssh-env,ssh-terminfo
```

**有効になる機能**:
- 新規タブ/スプリットで作業ディレクトリ継承
- プロンプト間ジャンプ (`jump_to_prompt`)
- Alt/Optionクリックでカーソル移動

## macOS固有設定

```bash
# タイトルバー
macos-titlebar-style = transparent  # 透過（デフォルト）
macos-titlebar-style = tabs         # タブ統合

# Optionキー
macos-option-as-alt = true          # Altとして扱う

# クイックターミナル
keybind = global:cmd+grave_accent=toggle_quick_terminal
quick-terminal-position = top
quick-terminal-size = 40%
quick-terminal-autohide = true

# 背景透過
background-opacity = 0.95
background-blur = 20
```

## ウィンドウ設定

```bash
window-padding-x = 12
window-padding-y = 8,12           # 上,下
window-padding-balance = true     # 自動バランス
```

## トラブルシューティング

### 設定が反映されない

```bash
ghostty +validate-config          # エラー確認
```

一部設定はランタイムリロード非対応。新規ウィンドウで確認。

### シェル統合が動作しない

```bash
# .zshrc に追加
if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
    source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"
fi
```

### テーマが見つからない

1.2.0以降は **Title Case** で指定:
- NG: `catppuccin-mocha`
- OK: `Catppuccin Mocha`

### globalキーバインドが効かない (macOS)

システム環境設定 > プライバシーとセキュリティ > アクセシビリティでGhosttyを許可。

## 詳細ドキュメント

包括的なリファレンスは `docs/ghostty-config-guidelines.md` を参照。
