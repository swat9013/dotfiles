---
paths: sheldon/**
---

# Sheldon プラグインマネージャー

## Oh-My-Zsh ではなく Sheldon

**選定理由**:
- 軽量: Rustで書かれており、起動が高速
- シンプル: TOML設定ファイルで管理
- 遅延読み込み: zsh-defer プラグインと組み合わせて起動時間を最適化

## 設定ファイル

- **場所**: `sheldon/plugins.toml`
- **リンク先**: `~/.config/sheldon/plugins.toml` (dotfilesLink.sh により自動リンク)

## プラグイン一覧

| プラグイン | 用途 | 読み込みタイミング |
|-----------|------|----------------|
| zsh-defer | 遅延読み込み、起動時間最適化 | 即時 |
| zsh-syntax-highlighting | コマンド構文ハイライト | 遅延 |
| zsh-completions | 高度な補完機能 | 遅延 |
| ohmyzsh-lib | Oh-My-Zshのlib部分のみ使用 | 即時 |

## plugins.toml 構成

```toml
[plugins.zsh-defer]
github = "romkatv/zsh-defer"

[plugins.zsh-syntax-highlighting]
github = "zsh-users/zsh-syntax-highlighting"
apply = ["defer"]

[plugins.zsh-completions]
github = "zsh-users/zsh-completions"
apply = ["defer"]

[plugins.ohmyzsh-lib]
github = "ohmyzsh/ohmyzsh"
dir = "lib"
```

## 初期化

`.zshrc` で以下のように初期化:

```bash
eval "$(sheldon source)"
```

## キャッシュ管理

Sheldon は自動でキャッシュを生成し、2回目以降の起動を高速化。

### キャッシュクリア

```bash
sheldon lock --update
```

## プラグイン追加手順

1. `sheldon/plugins.toml` を編集
2. プラグインエントリを追加
3. `.zshrc` を再読み込み (`exec zsh` または新規シェル起動)

## gitignore

`sheldon/repos/` はgitignore対象（プラグインのGitリポジトリが保存される）
