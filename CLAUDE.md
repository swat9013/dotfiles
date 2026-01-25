# CLAUDE.md

## リポジトリ概要

macOS/Linux開発環境のdotfiles。シンボリックリンク方式でgit管理。

## 主要コマンド

```bash
./lib/dotfilesLink.sh       # シンボリックリンク作成・再作成
brew bundle --global        # Homebrewパッケージ更新
```

## 技術スタック

- Zsh + Sheldon（プラグイン管理）
- ghostty（ターミナル、Tokyo Night）
- yazi（ファイルマネージャー）
- Claude Code（.claude-global/に集約）

## アーキテクチャ概要

- `.`で始まるファイル → `~`へシンボリックリンク
- `sheldon/`, `ghostty/`, `yazi/` → `~/.config/`配下
- `.claude-global/` → `~/.claude/`配下

### Claude Code設定の3層構造

| 層 | 用途 | 適用タイミング |
|----|------|--------------|
| CLAUDE.md | プロジェクト全体の方針 | 常時 |
| rules/ | パス固有のガイドライン | 該当パスのファイルアクセス時 |
| skills/ | ワークフロー（サブエージェント呼び出し含む） | 明示的呼び出し or キーワードマッチ |

**重要**: `.claude/agents/` は使用しない。サブエージェントはskill内でTask toolを使ってadhocに呼び出す。

### docs/ と rules/ の責務分担

| 観点 | docs/ | rules/ |
|------|-------|--------|
| 役割 | 正（マスター）、詳細リファレンス | Claude向け凝縮ルール |
| 対象読者 | 人間 | Claude |
| 記述スタイル | 詳細・読みやすい | 高シグナル・簡潔 |
| 参照関係 | - | docs/を参照しない（自己完結） |
| 重複 | 許容（用途別最適化優先） | 許容 |

## コーディング原則

- シンプルさ優先（KISS）
- 既存パターンに従う（新規ツールは`.zsh/[tool].zsh`）
- 詳細は各`rules/`ファイルを参照

## Gotchas

- **rules の paths 記法**: YAML配列（ハイフン複数行）は適用されない。カンマ区切りワンライナー、クオーテーションなしで記述
  ```yaml
  paths: **/*.test.*, **/*.spec.*
  ```
- **macOS lsof の複数条件**: `-a`フラグ必須（AND条件）。`lsof -a -d cwd -p $pid`
- **コマンド名衝突**: `cc*` 系は既存エイリアス確認必須（例: `ccs`=`claude --model sonnet`）
- **サブエージェント残存**: Task toolで起動したサブエージェントは親セッション終了後も残存しメモリ消費。定期的に`cck --sub`でクリーンアップ
- **zsh サブシェル内 PATH**: `$()` 内で `local var=$(dirname ...)` は PATH 解決失敗の可能性。フルパス `/usr/bin/dirname` を使用
- **zsh NULLCMD の罠**: `> file` だけの行は `cat > file` として実行される（NULLCMD デフォルト）。stdin待ちでハング。解決策: `: > file`
