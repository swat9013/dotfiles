---
paths: sheldon/**
---

# Sheldon プラグインマネージャー

## プラグイン一覧

| プラグイン | 用途 | 読み込み方式 |
|-----------|------|------------|
| zsh-defer | 遅延読み込み・起動時間最適化 | 即時 |
| zsh-syntax-highlighting | コマンド構文ハイライト | 遅延 |
| zsh-completions | 高度な補完機能 | 即時 |
| zsh-abbr | 略語展開 | 即時 |
| ohmyzsh-lib | Oh-My-Zsh の lib 部分のみ使用 | 即時 |

## キャッシュ

`sheldon source > ~/.cache/sheldon/sheldon.zsh` でキャッシュ生成。`plugins.toml` が新しい場合のみ再生成される。

## プラグイン追加時の注意

`sheldon lock --update` 実行後、新しいシェルを起動して反映する。

## gitignore

`.sheldon/plugins.lock` および `.sheldon/repos/` は gitignore 済み。

詳細は `zsh` スキルを参照。
