---
paths: .Brewfile
---

# Brewfile パッケージ管理

## 概要

Homebrew Bundle を使用して、50以上のパッケージを一元管理。

## パッケージカテゴリ

### CLI ツール

| パッケージ | 用途 |
|----------|------|
| asdf | バージョン管理（Ruby、Node.jsなど） |
| bun | 高速JavaScriptランタイム |
| emacs | エディタ |
| git | バージョン管理 |
| jq | JSON処理 |
| fzy | インタラクティブフィルタ（peco後継） |
| ripgrep | 高速grep |
| sheldon | Zshプラグインマネージャー |
| tmux | ターミナルマルチプレクサ |
| uv | Python パッケージマネージャー |
| zsh | シェル |

### AI/開発ツール

| パッケージ | 用途 |
|----------|------|
| ollama | ローカルLLM実行 |
| poetry | Python 依存関係管理 |
| uv | 高速 Python パッケージマネージャー |

### GUI アプリケーション

| パッケージ | 用途 |
|----------|------|
| dash | API ドキュメントビューア |
| dbeaver-community | データベースクライアント |
| devtoys | 開発者向けユーティリティ |
| obs | 画面録画・配信 |
| codex | コードスニペット管理 |

### フォント

| パッケージ | 用途 |
|----------|------|
| font-iosevka | プログラミング用等幅フォント |
| font-monaspace | モダンな等幅フォント |
| font-myrica | 日本語対応等幅フォント |

## 管理コマンド

### インストール

```bash
brew bundle --global
```

`.Brewfile` を `~/.Brewfile` にリンクして使用。

### 更新

```bash
brew bundle --global --cleanup
```

- `--cleanup`: Brewfileに記載されていないパッケージを削除

### 現在のパッケージをBrewfileに反映

```bash
brew bundle dump --global --force
```

## Brewfile の構成

```ruby
# CLI tools
brew "asdf"
brew "git"
...

# GUI apps
cask "dash"
cask "dbeaver-community"
...

# Fonts
cask "font-iosevka"
cask "font-monaspace"
...
```

## パッケージ追加手順

1. `.Brewfile` を編集
2. `brew bundle --global` でインストール
3. 不要パッケージがあれば `brew bundle --global --cleanup` でクリーンアップ
