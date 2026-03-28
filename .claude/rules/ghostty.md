---
paths: ghostty/**
---

# Ghostty設定ガイド

## コマンド早見表

```bash
ghostty +show-config                   # 現在の設定表示
ghostty +show-config --default --docs  # デフォルト設定（ドキュメント付き）
ghostty +validate-config               # 設定検証
ghostty +list-themes                   # テーマ一覧
ghostty +list-fonts                    # フォント一覧
ghostty +list-keybinds                 # キーバインド一覧
ghostty +list-actions                  # アクション一覧
```

## 設定ファイル

**場所**: `~/.config/ghostty/config`（dotfiles: `ghostty/config` → シンボリックリンク）

```bash
key = value        # 基本形式
# コメント         # 行頭のみ有効
font-family =      # 空値でデフォルトにリセット
config-file = ?local.conf  # ? で存在しなくてもエラーなし
```

## トラブルシューティング

**設定が反映されない**: `ghostty +validate-config` でエラー確認。一部設定はランタイムリロード非対応 → 新規ウィンドウで確認。

**テーマが見つからない**: 1.2.0以降は Title Case で指定（例: `Catppuccin Mocha`、NG: `catppuccin-mocha`）。

**シェル統合が動作しない**: `.zshrc` に手動追加:

```bash
if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
    source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"
fi
```

**globalキーバインドが効かない (macOS)**: システム環境設定 > プライバシーとセキュリティ > アクセシビリティでGhosttyを許可。

**Homebrew アップグレードエラー** (`/Applications/Ghostty.app` が存在しない):

```bash
brew uninstall --cask ghostty@tip --force
brew install --cask ghostty@tip
```

---

詳細は `ghostty` スキルを参照。
