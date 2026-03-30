# Zsh エイリアス・キーバインド設計リファレンス

エイリアス・関数・キーバインドの一覧はソースファイル（`.zsh/aliases.zsh`, `.zsh/keybinds.zsh`, `zsh-abbr/user-abbreviations`）を直接参照すること。
このドキュメントは「なぜそうなっているか」「どう判断するか」の設計判断に焦点を当てる。

## TOC

1. [abbr vs alias 判断フロー](#1-abbr-vs-alias-判断フロー)
2. [fzf 統合の実装方式](#2-fzf-統合の実装方式)
3. [$CLAUDECODE による挙動制御](#3-claudecode-による挙動制御)
4. [Gotchas](#4-gotchas)

---

## 1. abbr vs alias 判断フロー

| 基準 | abbr (zsh-abbr) | alias |
|------|-----------------|-------|
| 展開 | スペース/Enter で展開 → 履歴にフルコマンドが残る | 展開されない |
| Tab補完 | v5.2.0+ でabbr名補完対応。展開後補完は非標準 | 完全対応 |
| 定義場所 | `zsh-abbr/user-abbreviations` | `.zsh/aliases.zsh` |

**判断優先順位**:
1. 引数処理・条件分岐が必要 → **関数**
2. Tab補完が必要 / 3トークン以上 / コマンド上書き（`rm='rmtrash'`等） → **alias**
3. それ以外（単純な短縮形） → **abbr**（履歴にフルコマンドが残る利点）

---

## 2. fzf 統合の実装方式

### CSI u シーケンス

Ctrl+Shift の組み合わせを区別するため CSI u プロトコルを使用。Ghostty はデフォルトで送信するため追加設定不要。

| キー | シーケンス | コード根拠 |
|------|-----------|-----------|
| Ctrl+Shift+E | `\e[101;6u` | e=101（ASCII 小文字 e）, modifier 6=Ctrl+Shift |
| Ctrl+Shift+F | `\e[102;6u` | f=102 |
| Ctrl+Shift+G | `\e[103;6u` | g=103 |

### Ctrl+r（頻度順履歴検索）の設計

`\history -n 1 | tail -r | awk '!seen[$0]++'` で重複除去 + 最新順に並べ替え後、`fzf --no-sort --scheme=history` に渡す。`--no-sort` で fzf の並べ替えを無効化し、使用頻度順を維持。

### Ctrl+Shift+G（動的 grep）の設計

`fzf --disabled --bind "change:reload:rg ... {q}"` で入力変更のたびに rg を再実行。`bat --highlight-line {2}` でマッチ行をハイライト。エディタ起動は `$EDITOR` の値で分岐（VS Code → `code --goto file:line`、それ以外 → `$EDITOR +line file`）。

---

## 3. $CLAUDECODE による挙動制御

Claude Code 実行中は `$CLAUDECODE` 環境変数がセット済み。シェルのノイズ出力を抑制:

| 挙動 | 通常 | `$CLAUDECODE` セット時 |
|------|------|----------------------|
| `chpwd` フック | `ls` を自動実行 | 無効化 |
| 空行 Enter | ls + git status 表示 | 無効化 |

```zsh
function chpwd() { [[ -z $CLAUDECODE ]] && ls }
# custom_accept_line 内も同様の条件分岐
```

---

## 4. Gotchas

- **cc* 系コマンド名衝突**: `cc`=`claude`, `cca`, `cco`, `ccs`, `ccp` が定義済み。新規 `cc*` エイリアス追加時は衝突確認必須
- **rm → rmtrash の設計意図**: 誤削除防止。完全削除が必要な場合は `command rm` または `/bin/rm`
- **e() 関数の TERM 設定**: Ghostty の `xterm-ghostty` terminfo が Emacs デーモンに伝播すると SGR マウスプロトコル不整合が発生するため、`TERM=xterm-256color` でフォールバック
