---
name: log-designing
description: >-
  ログ設計の知識系スキル。構造化ログ・ログレベル・CLIパターンの設計判断基準を提供する。
  「ログ設計」「構造化ログ」「ログ基盤」「ログレベル」「verbosity」と依頼された時に参照する。
user-invocable: false
---

# Log Designing

## 1. ログ設計 7原則

| # | 原則 | 要点 |
|---|------|------|
| 1 | 構造化ファースト | JSON構造化。TTY検出でpretty/1行を自動切替 |
| 2 | セマンティックエラーコード | カテゴリ別コード（E4xxx/E5xxx/E6xxx）で修正戦略を即決 |
| 3 | 再現情報の完全性 | 入力値・期待値vs実際値・スタックトレース・実行コンテキスト |
| 4 | 相関ID | correlation_id/trace_id で因果追跡。CLIはTRACEPARENT環境変数 |
| 5 | stdout/stderr分離 | stdout=データ出力（パイプ対象）、stderr=ログ/進捗/エラー |
| 6 | 段階的verbosity | -v/-vv/-vvv/-q で INFO/DEBUG/TRACE/ERROR |
| 7 | 信号対雑音比 | 意思決定に寄与するログのみ。未使用ログは定期削除 |

## 2. CLIパターン

### stdout/stderr 分離の判断基準

- リダイレクト後も見て欲しい → stderr
- 実行結果として保存したい → stdout
- 迷ったら stderr（安全側）

### verbosity 制御

| フラグ | レベル | 出力内容 |
|--------|--------|---------|
| (なし) | WARNING | 重要な情報のみ |
| -v | INFO | 実行ステップ、構成詳細 |
| -vv | DEBUG | API呼び出し、中間値 |
| -vvv | TRACE | 内部状態、すべての詳細 |
| -q | ERROR | エラーのみ（自動化向け） |

環境変数フォールバック（`LOG_LEVEL`, `RUST_LOG`等）も提供する。

## 3. ライブラリ早見表

| 言語 | 推奨 | 理由 |
|------|------|------|
| Python | structlog | JSON対応、文脈管理 |
| Node.js | Pino | JSON優先、高速 |
| Go | slog（標準） | Go 1.21+標準 |
| Rust | tracing | async対応、スパン構造 |

## 4. 段階的導入フロー

- Phase 1（即効）: JSON構造化ログ導入 + エラーに入力値/スタックトレース記録
- Phase 2（最適化）: 相関ID + エラーコード体系 + テスト内ログキャプチャ
- Phase 3（スケール）: OpenTelemetry統合 + ログ集約プラットフォーム
