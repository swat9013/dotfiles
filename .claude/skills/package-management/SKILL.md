---
name: package-management
user-invocable: false
description: |-
  dotfilesのmacOSパッケージ管理（Brewfile/mise/mas）の設計判断・分類基準・罠を提供する知識ベーススキル。
  Use when「Brewfile」「パッケージ管理」「mise」「mas」「brew install」「ツール追加」「パッケージ追加」。
---

# Package Management

## バージョン情報

現在: !`~/.dotfiles/.claude/skills/package-management/scripts/versions.sh`
記録: brew 5.1.1 | mise 2026.3.17 | mas 6.0.1

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

## 管理体系の設計判断

| 層 | ツール | 管理対象 | 選定理由 |
|----|--------|---------|----------|
| システムツール | Homebrew (brew) | CLI、ライブラリ | macOS標準。依存解決が堅牢 |
| GUIアプリ | Homebrew (cask) | .app | brew統合で一元管理 |
| App Store | mas | App Store購入済みアプリ | caskにない配布形態。Apple ID紐付き |
| 言語バージョン | mise | Node/Python/Ruby等 | asdfから移行。7倍高速、direnv代替も内蔵 |

**Homebrew Bundleで管理しないもの**: システム設定（defaults write）、GUI設定値、サブスクリプション型アプリ（Setapp等）

## 新ツール追加の判断フロー

```
ツール追加依頼
├─ 言語ランタイム/バージョン管理が必要？ → mise (.tool-versions)
├─ App Storeで配布？ → mas (Brewfile末尾)
├─ GUIアプリ（.app）？ → cask (Brewfile「GUI apps」セクション)
└─ CLIツール → brew (Brewfile、該当カテゴリ)
```

## Brewfile カテゴリ設計

**分類基準**: 用途・ドメイン単位。各カテゴリ内はアルファベット順ソート。

| 判断基準 | 例 |
|---------|-----|
| シェル環境に直結 → Shell | zsh, sheldon, starship, tmux |
| Git操作に使う → Git | git, gh, lazygit |
| 開発言語・ビルド → Dev tools | mise, go, uv, bun |
| クラウド・コンテナ → Infrastructure | awscli, colima, deck |
| 上記に当てはまらない → Other | libyaml, yarn |

**新カテゴリ追加**: 同ドメインのツールが3つ以上になったら検討。

## Gotchas

| 罠 | 説明 |
|----|------|
| Brewfile.lock.json | バージョンロックではなくデバッグ情報。再現性保証に使えない |
| mas の Apple ID 制限 | 一度も購入していないアプリはインストール不可。新マシンでは事前にApp Store手動購入が必要 |
| `brew bundle --cleanup` の破壊性 | Brewfileにないパッケージを削除する。`--force`なしなら確認あり |
| mise activate vs shims | activate推奨（PATH動的切替）。shims は120msオーバーヘッド。`.zsh/mise.zsh`でキャッシュ方式を使用中。miseアップデート時はキャッシュ手動削除が必要 |
| direnv削除済み | direnvは完全削除（2026-03-30）。環境変数管理はmise `[env]` に一本化。詳細は `mise` スキル参照 |
| tap の明示管理 | サードパーティ tap は Brewfile 先頭で明示宣言。暗黙依存を防ぐ |

## 詳細リファレンス

| ファイル | 内容 |
|---------|------|
| `references/brewfile-conventions.md` | カテゴリ設計の理由、ソート規則、tap管理、VS Code拡張・フォント管理 |
| `references/mise-and-direnv.md` | asdf→mise移行の経緯、direnv併用判断、activate方式の選定理由 |
