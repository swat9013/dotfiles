# コンテキスト圧縮ガイドライン

CLAUDE.md が肥大化した際の圧縮方針。

## 圧縮タイミング

- 推奨: 200行超過時
- 必須: 300行超過時

規模チェック:
```bash
python3 ~/.dotfiles/.claude-global/skills/_shared/scripts/measure-context.py
```

## 圧縮方針

### 1. 重複統合

類似項目を統合し、冗長性を排除。

**例**:
```diff
- rm コマンドは rmtrash を使用
- 削除時は rmtrash で安全に
+ rm → rmtrash で誤削除防止
```

### 2. 表現簡潔化

冗長な説明を短縮し、表形式を活用。

**例**:
```diff
- このプロジェクトでは TypeScript を使用しています。
- フレームワークとして Next.js 14 を採用しています。
- データベースには PostgreSQL を使います。
+ | 技術 | 選択 |
+ |------|------|
+ | 言語 | TypeScript |
+ | FW | Next.js 14 |
+ | DB | PostgreSQL |
```

### 3. 詳細分離

長い説明は docs/ へ分離し、CLAUDE.md にはサマリーと参照リンクのみ。

**例**:
```diff
- ## 認証フロー
- 1. ユーザーがログインフォームに入力
- 2. サーバーがJWTトークンを生成
- 3. クライアントがトークンを保存
- （詳細30行）

+ ## 認証フロー
+ JWT ベース。詳細: docs/guidelines/auth-flow.md
```

### 4. 古い情報削除

陳腐化した内容を削除。

**チェック項目**:
- 使われなくなった技術・ライブラリ
- 解決済みの制約・問題
- 古いバージョンの情報

## Progressive Disclosure

**原則**: 頻用する情報を CLAUDE.md に、詳細を docs/ に。

| 頻度 | 保存先 |
|------|--------|
| 毎回参照 | CLAUDE.md |
| 時々参照 | docs/guidelines/ |
| 一度だけ | docs/decisions/ADR-xxx.md |

## 成功基準

1. ✅ 200行以下に収まっている
2. ✅ 重複・冗長性が排除されている
3. ✅ 詳細が適切に分離されている
4. ✅ 最新の情報のみが残っている
