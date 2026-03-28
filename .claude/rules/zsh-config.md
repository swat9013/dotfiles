---
paths: .zshenv, .zshrc, .zsh/**, zsh-abbr/user-abbreviations
---

# Zsh設定

## 起動ファイルの役割

| ファイル | 役割 |
|----------|------|
| `.zshenv` | 環境変数（EDITOR, LANG, CLICOLOR）、Homebrew初期化。全シェルで読み込まれる |
| `.zshrc` | エイリアス、関数、プロンプト、キーバインド、補完。対話シェルのみ |
| `.zsh/*.zsh` | モジュール別設定（aliases.zsh, keybinds.zsh, opt.zsh 等11ファイル） |

## モジュール化指針

新ツール設定は `.zsh/[tool].zsh` に追加する。ファイル名は小文字+アンダースコア。

## abbr vs alias の判断基準

| 基準 | 選択 |
|------|------|
| 引数を後ろに付けて使う短縮形（`g status`等） | abbr（履歴にフルコマンドが残る） |
| デフォルトオプション付与・複合コマンド | alias |
| Tab補完が必要・展開後が長い（3トークン以上） | alias |

## Gotchas

- **サブシェル内PATH**: `$()` 内でPATH解決失敗の可能性 → フルパスを使用（例: `/opt/homebrew/bin/fd`）
- **NULLCMDの罠**: `> file` 単独行は `cat > file` として実行されstdinを待つ → `: > file` を使う
- **`cc*`系エイリアス追加時**: 既存との衝突確認必須（例: `ccs`=`claude --model sonnet` 定義済み）
- **`.zshenv`に出力コマンド禁止**: scp等の非対話シェルで誤動作する

詳細は `zsh` スキルを参照。
