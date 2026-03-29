# CLAUDE.md

## リポジトリ概要

macOS/Linux開発環境のdotfiles。シンボリックリンク方式でgit管理。

## 主要コマンド

```bash
./lib/dotfilesLink.sh       # シンボリックリンク作成・再作成
brew bundle --global        # Homebrewパッケージ更新
```

## 技術スタック

- Zsh + Sheldon（プラグイン管理）+ zsh-abbr（略語展開）
- ghostty（ターミナル、Tokyo Night）+ Starship（プロンプト）
- Colima（コンテナランタイム）
- Zed（エディタ）
- Claude Code（グローバルスキル: .claude-global/、プロジェクト固有スキル: .claude/skills/）

## アーキテクチャ概要

- `.`で始まるファイル → `~`へシンボリックリンク
- `sheldon/`, `ghostty/`, `starship/`, `zsh-abbr/` → `~/.config/`配下
- `zed/` → `~/.config/zed/`配下
- `colima/colima.yaml` → `~/.colima/default/`配下
- `.claude-global/` → `~/.claude/`配下

**重要**: `.claude/agents/` は使用しない。サブエージェントはskill内でAgent toolを使ってadhocに呼び出す。

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
- 同じ指示を CLAUDE.md と rules/ に重複記載しない

