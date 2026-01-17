# 最小 CLAUDE.md テンプレート

## テンプレート（20-30行）

```markdown
# CLAUDE.md

## プロジェクト概要

{プロジェクト名}は{目的}のための{種類}。

## 技術スタック

- 言語: {language}
- フレームワーク: {framework}
- 主要ライブラリ: {libraries}

## コマンド

```bash
{build_command}     # ビルド
{test_command}      # テスト
{lint_command}      # lint
```

## 核心原則

- {principle_1}
- {principle_2}
- {principle_3}
```

---

## 例: Web API プロジェクト

```markdown
# CLAUDE.md

## プロジェクト概要

user-api は社内ユーザー管理のための REST API。

## 技術スタック

- 言語: TypeScript
- フレームワーク: Express.js
- DB: PostgreSQL + Prisma

## コマンド

```bash
npm run build       # ビルド
npm test            # テスト
npm run lint        # lint
```

## 核心原則

- 厳格な型付け（any 禁止）
- エラーは明示的にハンドリング
- テストカバレッジ80%以上維持
```

---

## 例: CLI ツール

```markdown
# CLAUDE.md

## プロジェクト概要

dotfiles は開発環境設定を管理する個人用リポジトリ。

## 技術スタック

- シェル: Zsh + Sheldon
- パッケージ: Homebrew
- 設定配布: シンボリックリンク

## コマンド

```bash
./lib/dotfilesLink.sh    # シンボリックリンク作成
brew bundle --global     # パッケージ更新
```

## 核心原則

- シンプルな設定を維持
- プラットフォーム差異を吸収
- rm → rmtrash で安全削除
```

---

## 避けるべき内容

CLAUDE.md に含めない:

- 特定パスにのみ適用されるルール → `rules/`
- 3ステップ以上のワークフロー → `skills/`
- ユーザー実行のテンプレート → `commands/`
- 詳細なAPI仕様 → 外部ドキュメント参照
