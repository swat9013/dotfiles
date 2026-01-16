---
paths:
  - .gitconfig
  - .gitconfig.local.sample
---

# Git設定アーキテクチャ

## 設定ファイル分離

| ファイル | 用途 | バージョン管理 |
|---------|------|--------------|
| `.gitconfig` | 共通設定（エイリアス、カラー、動作設定） | ✓ Git管理 |
| `.gitconfig.local` | 個人情報（名前、メール、署名キー） | ✗ gitignore |
| `.gitconfig.local.sample` | テンプレート | ✓ Git管理 |

### 初期設定手順

```bash
cp .gitconfig.local.sample .gitconfig.local
# .gitconfig.local を編集してユーザー名・メールを設定
```

## 主要エイリアス

| エイリアス | 機能 | 詳細 |
|-----------|------|------|
| `cofeature <name>` | feature/\<name\>ブランチ作成 | 機能開発用ブランチ |
| `cofix <name>` | fix/\<name\>ブランチ作成 | バグ修正用ブランチ |
| `delete-merged-branch` | マージ済みブランチ削除 | ローカルブランチのクリーンアップ |
| `aicommit` | Claude でConventional Commits生成 | AIによるコミットメッセージ作成 |

### aicommit の仕組み

Claude Code CLI を使用して、変更内容から適切なコミットメッセージを自動生成。Conventional Commits形式（`feat:`, `fix:`, `docs:`など）に準拠。

## セキュリティ

### gitignore対象

- `.gitconfig.local`: 個人情報を含むため追跡しない
- `.env`, `*.key`: Claude Code設定でRead deny

### シークレット管理方針

1. **個人情報**: `.gitconfig.local` に記載（名前、メール、GPGキー）
2. **テンプレート**: `.gitconfig.local.sample` を提供
3. **Claude Code**: permissions設定でシークレットファイルへのアクセスを制限

```json
// .claude-global/settings.json
{
  "permissions": {
    "deny": [
      "Read(./.env)",
      "Read(**/*.key)"
    ]
  }
}
```

## 60以上のGitエイリアス

`.gitconfig` には60以上のエイリアスが定義されており、以下のカテゴリに分類される:

- ブランチ操作
- コミット操作
- ログ表示
- リベース・マージ
- リモート操作
- ファイル操作
