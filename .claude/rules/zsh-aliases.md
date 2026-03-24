---
paths: .zsh/aliases.zsh, zsh-abbr/user-abbreviations
---

# Zshエイリアス・abbr設計

## abbr と alias の使い分け

| 基準 | abbr (zsh-abbr) | alias |
|------|-----------------|-------|
| 用途 | コマンド名の短縮（引数を後ろに付けて使う） | オプション付き・パイプ・引数不要のコマンド |
| 展開 | スペース/Enterで展開→履歴にフルコマンドが残る | 展開されない |
| Tab補完 | abbr名の補完はv5.2.0+対応。展開後コマンドの補完は非標準 | 完全対応 |
| 定義場所 | `zsh-abbr/user-abbreviations` | `.zsh/aliases.zsh` |

### 判断フロー

1. **引数処理・条件分岐が必要？** → 関数
2. **Tab補完が必要？** → alias
3. **展開後が長い（3トークン以上）？** → alias（展開表示がノイジー）
4. **既存コマンドの上書き？** → alias（`rm='rmtrash'`等）
5. **グローバル展開（パイプ等）？** → alias -g / global abbr
6. **上記いずれでもない短縮形？** → abbr（履歴・学習・共有の恩恵）

### 例

```bash
# abbr: 短縮名→コマンド名。引数を後ろに付ける
abbr g="git"        # g status → git status（履歴に git status が残る）
abbr tm="tmux"      # tm a → tmux a

# alias: オプション付き・複合コマンド・引数不要
alias grep='grep --color=auto'   # デフォルトオプション付与
alias relogin="exec $SHELL -l"   # 引数不要・Tab補完が必要
alias dclog='docker-compose logs -f'  # 複合コマンド
```

## カテゴリ別エイリアス

| カテゴリ | 例 | 用途 |
|---------|-----|------|
| 標準コマンド | `grep='grep --color=auto'` | カラー出力有効化 |
| エディタ | `e()` (emacsclient) | エディタ起動の短縮 |
| Git | `L`, `LA`, `R` | fzfによるコミット・reflog選択 |
| Tmux | `ide` | Tmux起動・レイアウト管理 |
| Docker | `dcew`, `dcrw`, `dclog` (20+) | Docker Compose操作 |
| Rails | `con`, `db_migrate` | Rails開発支援 |
| 削除保護 | `rm='rmtrash'` | 誤削除防止 |
| VPN/Kanban | `fvpn`, `vk` | VPN接続、Kanbanサーバー |

## 重要な設計判断

### rm → rmtrash

```bash
alias rm='rmtrash'
```

**理由**: 誤削除防止。ファイルをゴミ箱に移動（完全削除を防ぐ）

### エディタ関数 e()

```bash
e() {
  emacsclient --create-frame "$@"
}
```

**理由**: Emacsデーモンモードでの新規フレーム作成。起動高速化

### Git関連

- `g`: abbr（`abbr g="git"`）。引数を付けて使う短縮形
- `L`, `LA`, `R`: グローバルalias。fzfによるコミット・reflog選択

### Docker Compose エイリアス (20+)

命名規則:
- `dc*`: docker compose コマンド群
- `dcew`: docker compose exec web
- `dcrw`: docker compose run web
- `dclog`: docker compose logs

150以上のエイリアスを定義しており、頻繁に使用するコマンドを短縮化している。
