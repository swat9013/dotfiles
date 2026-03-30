# Zsh設定アーキテクチャ リファレンス

実装の詳細（モジュール一覧、plugins.toml構成等）はソースファイルを直接参照すること。
このドキュメントは「なぜそうなっているか」「どう拡張するか」の設計判断に焦点を当てる。

## TOC

1. [読み込みフローと設計判断](#1-読み込みフローと設計判断)
2. [Sheldonキャッシュ方式の設計理由](#2-sheldonキャッシュ方式の設計理由)
3. [パフォーマンス最適化テクニック](#3-パフォーマンス最適化テクニック)
4. [拡張ガイドライン](#4-拡張ガイドライン)
5. [Gotchas](#5-gotchas)
6. [参考資料](#6-参考資料)

---

## 1. 読み込みフローと設計判断

### `.zshenv` の設計判断

全シェルセッション（スクリプト含む）で読み込まれるため、以下のルールに従う:
- **出力コマンド禁止**: `echo`/`tput`等は scp 等の非対話シェルで誤動作する
- **環境変数のみ**: EDITOR判定（TERM_PROGRAM分岐）、ロケール、Homebrew初期化
- **プラットフォーム分岐**: `uname` で Darwin/Linux を検出し、各環境のPATH設定を分離

### `.zshrc` の読み込み順序の意図

```
1. typeset -U       → PATHの重複排除を最初に（以降のPATH追加が安全になる）
2. auto_update.sh   → &! でバックグラウンド完全非同期（起動時間に影響しない）
3. compinit         → Sheldonより前（プラグインが補完に依存する場合がある）
4. Sheldon          → キャッシュファイル方式（§2参照）
5. starship         → プロンプト初期化（Sheldonのafter hookで上書きされないよう後置）
6. .zsh/*.zsh       → モジュール群（アルファベット順でsource）
7-11. PATH追加      → モジュール読み込み後（go/bin, asdf等）
12. Ghostty統合     → $GHOSTTY_RESOURCES_DIR 存在時のみ
```

### モジュール分離の方針

新ツール設定は `.zsh/[tool].zsh` に独立ファイルとして追加する。`.zshrc` が for ループで自動 source するため、ファイル作成だけで有効化される。命名は小文字+アンダースコア。

---

## 2. Sheldonキャッシュ方式の設計理由

`eval "$(sheldon source)"` はサブシェル実行でコスト発生。キャッシュファイルを直接 `source` することで回避:

```zsh
sheldon_cache="${XDG_CACHE_HOME:-$HOME/.cache}/sheldon/sheldon.zsh"
sheldon_toml="${XDG_CONFIG_HOME:-$HOME/.config}/sheldon/plugins.toml"
if [[ ! -r "$sheldon_cache" || "$sheldon_toml" -nt "$sheldon_cache" ]]; then
    sheldon source > "$sheldon_cache"
fi
source "$sheldon_cache"
```

**判断基準**: `plugins.toml` のタイムスタンプがキャッシュより新しい場合のみ再生成。通常起動ではファイル `source` のみで済む。

### zsh-defer による遅延読み込み

重いプラグイン（zsh-syntax-highlighting等）は `apply = ["defer"]` でプロンプト表示後にバックグラウンド初期化。`templates.defer` セクションでカスタムテンプレートを定義済み。

### プラグイン追加手順

1. `sheldon/plugins.toml` にエントリを追加
2. 重いプラグインは `apply = ["defer"]` を指定
3. `exec zsh`（キャッシュ自動再生成）または `sheldon lock --update`（強制更新）

---

## 3. パフォーマンス最適化テクニック

### compinit キャッシュ（~/.zcompdump）

```zsh
if [[ -n ~/.zcompdump(N.mh+24) ]]; then
    compinit        # 24時間以上経過: 再生成
else
    compinit -C     # 24時間未満: キャッシュ使用
fi
```

### direnv hookキャッシュ（`direnv.zsh`）

`~/.cache/direnv/hook.zsh` にキャッシュ。direnvアップデート後は手動削除して再生成。

### auto_update.sh バックグラウンド実行

`&!`（disown相当）で完全非同期化。24時間経過で自動 `git pull`。起動時間に影響しない。

---

## 4. 拡張ガイドライン

| やりたいこと | 手順 |
|------------|------|
| エイリアス追加 | `.zsh/aliases.zsh` の該当カテゴリセクションに追記 |
| 新規ツール設定 | `.zsh/[tool].zsh` を新規作成 |
| プラグイン追加 | `plugins.toml` にエントリ → 重いものは `apply = ["defer"]` → `exec zsh` |
| 環境変数（全シェル） | `.zshenv` に追加（出力コマンド禁止） |
| 環境変数（対話のみ） | `.zshrc` または `.zsh/*.zsh` に追加 |
| OS固有設定 | `.zshenv` / `.zshrc` 内の `uname` 分岐ブロックに追記 |

---

## 5. Gotchas

| 問題 | 対策 |
|------|------|
| **サブシェルPATH** — `$()` 内でHomebrew等のPATH解決失敗 | フルパス使用（例: `/opt/homebrew/bin/fd`） |
| **NULLCMD罠** — `> file` が stdin待ちハング | `: > file` を使う |
| **.zshenvに出力禁止** — scp等で誤動作 | 対話前提の処理は `.zshrc` に移動 |
| **Sheldonキャッシュ古い** — plugins.toml変更後 | `sheldon lock --update` で強制更新 |
| **direnvキャッシュ古い** — direnvアップグレード後 | `rm ~/.cache/direnv/hook.zsh` → `exec zsh` |
| **`.sheldon/repos/`** はgitignore対象 | `sheldon/plugins.toml` のみgit管理 |

---

## 6. 参考資料

- [Zsh 公式ドキュメント - Startup Files](https://zsh.sourceforge.io/Intro/intro_3.html)
- [Sheldon 公式ドキュメント](https://sheldon.cli.rs/)
- [Zsh | ArchWiki](https://wiki.archlinux.org/title/Zsh)
