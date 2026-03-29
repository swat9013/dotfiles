---
paths: .Brewfile
---

# Brewfile 編集ルール

## 必須事項

- 適切なカテゴリセクション内に配置する（カテゴリ判断は package-management スキル参照）
- カテゴリ内はアルファベット順でソート
- サードパーティ tap は Taps セクションに明示宣言
- mas エントリは `mas "AppName", id: <numeric_id>` 形式

## 管理コマンド

- インストール: `brew bundle --global`
- 同期（不要パッケージ削除）: `brew bundle --global --cleanup`
- 現状エクスポート: `brew bundle dump --global --force`

## 禁止事項

- パッケージ一覧のコメントアウト放置（不要なら行削除）
- バージョン指定（Homebrew はバージョンピニング非対応）
