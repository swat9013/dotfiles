---
name: zsh
description: このdotfilesのZsh設定（アーキテクチャ・エイリアス・キーバインド・Sheldon）を提供する知識ベーススキル。Use when「zsh設定」「エイリアス追加」「abbr」「キーバインド」「Sheldon」。
user-invocable: false
---

# Zsh 知識ベース

## 設定ファイルの責務

| ファイル | 責務 | 置くべきもの |
|---------|------|-------------|
| `.zshenv` | 全シェル共通の環境変数 | EDITOR, LANG, PATH初期化。**出力コマンド禁止**（scp等で誤動作） |
| `.zshrc` | 対話シェル設定 | compinit, Sheldon, Starship, `.zsh/*.zsh` のsource |
| `.zsh/[tool].zsh` | ツール別モジュール | エイリアス、関数、キーバインド。新ツール追加時はここに新規ファイル作成 |

---

## 起動フロー

```
.zshenv       ロケール・EDITOR・Homebrew初期化
└── .zshrc
    ├── typeset -U        パス重複排除
    ├── auto_update.sh &! バックグラウンド自動更新（24h）
    ├── compinit          補完初期化（24h キャッシュ）
    ├── sheldon source    プラグイン（キャッシュファイル方式）
    ├── starship init     プロンプト
    └── .zsh/*.zsh        モジュール群（アルファベット順）
```

---

## 設計判断基準

### abbr vs alias

| 基準 | 選択 |
|------|------|
| 引数を後ろに付けて使う短縮形（`g status`等） | abbr（履歴にフルコマンドが残る） |
| デフォルトオプション付与・コマンド上書き | alias |
| 引数処理・条件分岐が必要 | 関数 |

定義場所: abbr → `zsh-abbr/user-abbreviations`、alias → `.zsh/aliases.zsh`

### 新ツール追加

`.zsh/[tool].zsh` に新規ファイル作成。命名は小文字+アンダースコア。

### $CLAUDECODE による挙動制御

Claude Code 実行中は `$CLAUDECODE` がセット済み。以下を自動無効化:
- `chpwd` フック（通常: `ls` 自動実行）
- 空行Enter（通常: ls + git status 表示）

---

## トラブルシューティング

| 症状 | 対処 |
|------|------|
| compinit が遅い | `rm ~/.zcompdump && compinit` で再生成 |
| Sheldonキャッシュが古い | `sheldon lock --update` で強制更新 |
| abbr が展開されない | `abbr list` で登録確認 |

---

## Gotchas

- **サブシェル内PATH**: `$()` 内でPATH解決失敗 → フルパス使用（例: `/opt/homebrew/bin/fd`）
- **NULLCMDの罠**: `> file` 単独行が stdin待ちハング → `: > file` を使う
- **`cc*`系エイリアス**: `cc`=`claude` 等が定義済み。新規追加時は衝突確認必須

---

## 詳細リファレンス

| ファイル | 内容 |
|---------|------|
| `references/config-architecture.md` | Sheldonキャッシュ方式の設計理由・パフォーマンス最適化・拡張手順 |
| `references/aliases-keybinds.md` | abbr判断フロー詳細・fzf統合の実装方式・$CLAUDECODE制御の仕組み |
