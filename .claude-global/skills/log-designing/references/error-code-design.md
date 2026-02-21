# セマンティックエラーコード設計テンプレート

## 1. カテゴリ → 修正戦略マッピング

| カテゴリ    | コード範囲 | 例                                        | 修正戦略                                       |
|------------|----------|------------------------------------------|----------------------------------------------|
| Validation | E4001–E4099 | E4001_INVALID_EMAIL, E4002_SCHEMA_MISMATCH | 入力バリデーション修正、スキーマ整合                |
| Resource   | E4100–E4199 | E4100_NOT_FOUND, E4101_DUPLICATE          | 存在確認、一意性制約チェック                       |
| State      | E5001–E5099 | E5001_INVALID_STATE_TRANSITION, E5002_INSUFFICIENT_FUNDS | 状態遷移ロジック修正、前提条件チェック    |
| Integration | E6001–E6099 | E6001_EXTERNAL_SERVICE_DOWN, E6002_TIMEOUT | リトライ、フォールバック、サーキットブレーカー      |

### 命名規則

```
E{カテゴリ1桁}{連番3桁}_{SCREAMING_SNAKE_CASE}
例: E4001_INVALID_EMAIL
```

カテゴリ番号でエラーの性質と修正アプローチを即座に判別できることが最重要。
`E4xxx` → 呼び出し側の入力問題 / `E5xxx` → サービス内部の状態問題 / `E6xxx` → 外部依存の問題。

---

## 2. エラーログ必須フィールド定義

AIエージェントがログだけで問題を再現・修正できるための必須フィールド:

- [ ] `error_code` — セマンティックエラーコード（上記テンプレート準拠）
- [ ] `message` — 人間可読なエラー説明
- [ ] `input` — エラー発生時の入力値（PII除外済み）
- [ ] `expected` / `actual` — 期待値 vs 実際値
- [ ] `stack_trace` — 関数名 + ファイル + 行番号
- [ ] `context` — 実行コンテキスト（環境、バージョン、設定値）
- [ ] `correlation_id` — リクエスト追跡用ID（分散トレーシング）
- [ ] `suggestions` — 修正提案（判定可能な場合のみ）

最小構造例:

```json
{
  "error_code": "E4001_INVALID_EMAIL",
  "message": "Email format is invalid",
  "input": { "email": "user@" },
  "expected": "RFC 5322 compliant email",
  "actual": "user@",
  "correlation_id": "req-abc123",
  "context": { "service": "user-api", "version": "1.4.2" }
}
```

---

## 3. 運用ルール

- 新規エラーはカテゴリ内の次の連番を使用する（欠番は許容）
- 廃止済みコードは再利用しない（将来のログ解析との衝突防止）
- 1エラー = 1コード（複合エラーは個別エラーに分解してリストで返す）
- `suggestions` は確実に正しい場合のみ記載する（誤誘導防止）
