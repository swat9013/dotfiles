# .wtp.yml → .config/wt.toml 移行ガイド

## TOC

- [.wtp.yml フォーマット](#wtp-yml-フォーマット)
- [変換マッピング](#変換マッピング)
- [変換例](#変換例)
- [変換手順](#変換手順)
- [注意事項](#注意事項)

## .wtp.yml フォーマット

```yaml
version: "1.0"
defaults:
  base_dir: ../worktrees      # worktree配置先（wt移行時は無視）
hooks:
  post_create:
    - type: copy               # ファイルコピー
      from: .claude
      to: .claude
    - type: symlink             # シンボリックリンク
      from: .bin
      to: .bin
    - type: command             # コマンド実行
      command: npm install
```

## 変換マッピング

| .wtp.yml | .config/wt.toml | 備考 |
|----------|-----------------|------|
| `defaults.base_dir` | **変換しない** | wt デフォルトを維持 |
| `type: copy` (from/to) | `[pre-start]` の `cp -r` コマンド | `{{ primary_worktree_path }}` でソース指定 |
| `type: symlink` (from/to) | `[pre-start]` の `ln -snfv` コマンド | `{{ primary_worktree_path }}` でソース指定 |
| `type: command` (短時間) | `[pre-start]` | ブロッキング。依存インストール等 |
| `type: command` (長時間) | `[post-start]` | バックグラウンド。devサーバー等 |

**判断基準**: 後続の作業に必須 → `pre-start`、バックグラウンドでOK → `post-start`

## 変換例

### 入力: .wtp.yml（obsidian-vault の実例）

```yaml
hooks:
  post_create:
    - type: copy
      from: .claude
      to: .claude
```

### 出力: .config/wt.toml

```toml
[pre-start]
copy-claude = "cp -r {{ primary_worktree_path }}/.claude ."
```

### 複合例

```yaml
# .wtp.yml
hooks:
  post_create:
    - type: copy
      from: .claude
      to: .claude
    - type: copy
      from: .env
      to: .env
    - type: symlink
      from: .bin
      to: .bin
    - type: command
      command: npm install
    - type: command
      command: npm run dev
```

```toml
# .config/wt.toml
[pre-start]
copy-claude = "cp -r {{ primary_worktree_path }}/.claude ."
copy-env = "cp {{ primary_worktree_path }}/.env ."
link-bin = "ln -snfv {{ primary_worktree_path }}/.bin .bin"
install = "npm install"

[post-start]
dev = "npm run dev -- --port {{ branch | hash_port }}"
```

**変換時のポイント**:
- `npm run dev` は長時間実行 → `post-start` に配置
- `hash_port` フィルターでポート衝突を自動回避（wtp にはない機能）
- `.env` が gitignored なら `wt step copy-ignored` で代替可能

## 変換手順

1. `.wtp.yml` を読み取る
2. 各 `hooks.post_create` エントリを上記マッピングに従って変換
3. `.config/wt.toml` を生成（ディレクトリがなければ `mkdir -p .config`）
4. 動作確認: `wt hook show` で設定が正しくパースされるか確認
5. `.wtp.yml` を削除（`git rm .wtp.yml`）

## 注意事項

- **copy-ignored との使い分け**: `wt step copy-ignored` は `.gitignore` に含まれるファイルのみコピー。`.claude` のような git 管理ファイルは個別 `cp` が必要
- **テンプレート変数**: `{{ primary_worktree_path }}` はメインworktreeの絶対パス。コピー元の指定に使用
- **フック承認**: プロジェクトフック（`.config/wt.toml`）は初回実行時に承認プロンプトが表示される。CI では `--yes` を使用
- **base_dir の非変換**: wtp の `base_dir: ../worktrees` はwt では不要。wt は独自のworktree-pathテンプレート（デフォルト: リポジトリと同階層）を使用し、命名規則も異なる。wt デフォルトに従う
