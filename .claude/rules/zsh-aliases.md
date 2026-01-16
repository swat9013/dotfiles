---
paths: .zsh/aliases.zsh
---

# Zshエイリアス設計

## カテゴリ別エイリアス

| カテゴリ | 例 | 用途 |
|---------|-----|------|
| 標準コマンド | `grep='grep --color=auto'` | カラー出力有効化 |
| エディタ | `c='code'`, `e()` (emacsclient) | エディタ起動の短縮 |
| Git | `g='git'`, `L`, `LA`, `R` | Git操作の効率化 |
| Tmux | `tm`, `ide` | Tmux起動・レイアウト管理 |
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

### Git関連エイリアス

- `g='git'`: Git操作の基本短縮
- `L`, `LA`, `R`: fzyによるブランチ選択・切り替え

### Docker Compose エイリアス (20+)

命名規則:
- `dc*`: docker compose コマンド群
- `dcew`: docker compose exec web
- `dcrw`: docker compose run web
- `dclog`: docker compose logs

150以上のエイリアスを定義しており、頻繁に使用するコマンドを短縮化している。
