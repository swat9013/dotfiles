---
paths: .default-python-packages, .Brewfile
---

# Python CLIツール管理

## ツール選択

| ツール | 用途 | 状態 |
|-------|------|------|
| uv tool | グローバルCLIツール管理 | PRIMARY |
| uv run | PEP 723スクリプト実行 | 推奨 |
| poetry | プロジェクト依存管理 | 共存 |
| pip | グローバルインストール | 禁止 |
| pipx | - | 削除済み |

## 基本コマンド

- インストール: `uv tool install <pkg>`
- 更新: `uv tool upgrade --all`
- 一覧: `uv tool list`
- 削除: `uv tool uninstall <pkg>`

## 制約

- pip install でのグローバルインストール禁止
- .default-python-packages は空を維持（asdf自動インストール防止）
- pipx は削除済み、推奨しない
- CLIツール → uv tool、プロジェクト依存 → poetry/uv add

## 関連ファイル

- `.Brewfile`: uv は Homebrew 管理 (L52)
- `.zshrc`: ~/.local/bin が PATH に含まれている (L45)
- `plugin_update.command`: `uv tool upgrade --all` で自動更新
