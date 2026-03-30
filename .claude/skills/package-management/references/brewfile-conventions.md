# Brewfile 設計規約

## カテゴリ構成の設計理由

Brewfileは用途・ドメイン単位でカテゴリ分けし、`# カテゴリ名`コメントで区切る。

**なぜ用途単位か**: アルファベット順フラットリストだと「何のためのツールか」が不明。新ツール追加時に配置先を即座に判断できることを優先。

**カテゴリの粒度**: 3ツール以上で独立カテゴリ化。2以下は Other に配置し、増えたら分離。

## 記載順序

```
Taps → brew (カテゴリ別) → cask (GUI apps → Fonts) → vscode → mas
```

**理由**: tap は他の全エントリの前提。brew/cask/vscode/mas の順は Homebrew Bundle の処理順序に合わせている。

## ソート規則

- カテゴリ内: アルファベット順
- 例外: tap付きパッケージ（`oven-sh/bun/bun`等）はtapなし名称でソート位置を判断

## tap 管理

- サードパーティ tap は Brewfile 先頭の Taps セクションで明示宣言
- `brew "org/tap/formula"` 形式で暗黙的に tap が追加されるが、明示宣言で可視性を確保
- 未使用 tap が残らないよう `brew bundle --cleanup` で定期的に整理

## VS Code 拡張の管理判断

Brewfile の `vscode` エントリで管理。Settings Sync との二重管理リスクがあるが、Brewfile に含めることで新マシンセットアップ時の手動作業を排除。

**追加基準**: 開発に必須の拡張のみ。テーマ等の好み系は含めない（Settings Sync に委譲）。

## mas エントリの管理

- Brewfile 末尾に `mas "AppName", id: <numeric_id>` 形式で配置
- ID は `mas list` で取得（`mas search` は非推奨、結果が不安定）
- Apple ID で一度も購入していないアプリは mas でインストール不可 — 新マシンでは手動購入が前提

## コメント付き無効化

`# cask "temurin"` のように一時的に無効化する場合はコメントアウト。完全不要なら行削除。コメントアウトが3つ以上溜まったら棚卸し。
