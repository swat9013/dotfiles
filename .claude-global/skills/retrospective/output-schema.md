# retrospective 診断ログ出力スキーマ

## 概要

claude-config が `.claude/tmp/diagnostic-trace/` へ出力するJSONLログのフィールド定義。
retrospective の L6レンズ（判断基準混同検出）が消費する診断ログの契約仕様。

- **形式**: JSON Lines (`.jsonl`)、追記前提で1判断1行
- **保存パス**: `.claude/tmp/diagnostic-trace/{YYYY-MM-DD-HHMMSS}-{session-id}.jsonl`
- **git管理**: 対象外（`.claude/tmp/` 配下、既存運用と整合）
- **ローテーション**: セッション単位で新ファイル

## フィールド定義

| フィールド | 型 | 必須/任意 | 説明 | 値域・例 |
|-----------|---|---------|------|---------|
| `timestamp` | string (ISO 8601) | 必須 | 判断発生時刻 | `"2026-04-17T10:30:00+09:00"` |
| `session_id` | string | 必須 | セッション識別子 | `"2026-04-17-103000-abc123"` |
| `skill` | string | 必須 | 実行中スキル名 | `"retrospective"`, `"claude-config"` |
| `phase` | string \| null | 任意 | スキル内フェーズ（Phase N or Step N-M） | `"Phase 2"`, `"Step 3-1"`, `null` |
| `judgment_type` | string (enum) | 必須 | 判断の種別 | `"reference"` \| `"value"` \| `"mixed"` |
| `reference_cited` | string \| null | 任意 | 参照した情報源のアンカーまたはパス | `"#maintainability-harness"`, `"docs/harness-engineering-domain-model.md"`, `null` |
| `value_stated` | string \| null | 任意 | 直接述べた価値判断の文字列（本文引用） | `"可読性が高い方が望ましい"`, `null` |
| `rationale` | string | 必須 | 判断の根拠（1〜2文） | `"参照先の明示なく価値語彙が混入しているため mixed と判定"` |
| `tool_used` | string \| null | 任意 | 判断時に使用したツール名 | `"Read"`, `"Grep"`, `"Edit"`, `null` |
| `context_size` | integer \| null | 任意 | 判断時のコンテキストサイズ概算（トークン数） | `42000`, `null` |

## フィールド詳細

### `timestamp`

- **型**: ISO 8601 形式文字列
- **必須**: yes
- **説明**: 判断が発生した時刻。タイムゾーン付きで記録する。
- **値域**: ISO 8601 準拠（例: `"2026-04-17T10:30:00+09:00"`）

### `session_id`

- **型**: string
- **必須**: yes
- **説明**: セッション識別子。ファイル名に使用するタイムスタンプベースの識別子と一致させる。
- **値域**: `"{YYYY-MM-DD-HHMMSS}-{random-or-hash}"` 形式を推奨

### `skill`

- **型**: string
- **必須**: yes
- **説明**: 実行中のスキル名。スキルディレクトリ名と一致させる。
- **値域**: `"retrospective"`, `"claude-config"`, `"harness-tuning"` 等

### `phase`

- **型**: string | null
- **必須**: no（任意）
- **説明**: スキル内フェーズの識別子。スキルに複数フェーズがある場合に記録する。
- **値域**: `"Phase 1"`, `"Phase 2"`, `"Step 3-1"` 等。フェーズ不明または単一の場合は `null`

### `judgment_type`

- **型**: enum string
- **必須**: yes
- **説明**: 判断の種別。判断基準（reference）と価値判断（value）のどちらに基づくか、または混在か。
- **値域**:
  - `"reference"` — 情報源（ドキュメント・アンカー・パス）に基づく判断
  - `"value"` — 達成価値・目的関数・達成シグナルを直接述べた判断
  - `"mixed"` — referenceとvalueが混在（L6レンズの検出対象）

### `reference_cited`

- **型**: string | null
- **必須**: no（任意）
- **説明**: 参照した情報源のアンカーIDまたはファイルパス。`judgment_type` が `"reference"` または `"mixed"` の場合に記録を推奨。
- **値域**: アンカー形式（`"#maintainability-harness"`）またはファイルパス（`"docs/harness-engineering-domain-model.md"`）。参照なしは `null`

### `value_stated`

- **型**: string | null
- **必須**: no（任意）
- **説明**: 直接述べた価値判断の文字列（本文から引用）。`judgment_type` が `"value"` または `"mixed"` の場合に記録を推奨。
- **値域**: 任意の文字列。価値判断なしは `null`

### `rationale`

- **型**: string
- **必須**: yes
- **説明**: 判断の根拠を1〜2文で記述。L6レンズが混同検出に使用する主要フィールド。
- **値域**: 任意の文字列（空文字列不可）

### `tool_used`

- **型**: string | null
- **必須**: no（任意）
- **説明**: 判断時に使用したツール名。判断のトレーサビリティのために記録する。
- **値域**: `"Read"`, `"Grep"`, `"Edit"`, `"Bash"`, `"Write"` 等。ツール未使用は `null`

### `context_size`

- **型**: integer | null
- **必須**: no（任意）
- **説明**: 判断時のコンテキストサイズ概算（トークン数）。コンテキスト圧迫による判断品質劣化の相関分析に使用する。
- **値域**: 0以上の整数。概算値可。不明の場合は `null`

## サンプルJSONLレコード

### reference 判断の例

```jsonl
{"timestamp":"2026-04-17T10:30:00+09:00","session_id":"2026-04-17-103000-abc123","skill":"retrospective","phase":"Phase 2","judgment_type":"reference","reference_cited":"#maintainability-harness","value_stated":null,"rationale":"maintainability-harnessの達成シグナル定義を参照してコードの可読性を評価した","tool_used":"Read","context_size":38000}
```

### value 判断の例

```jsonl
{"timestamp":"2026-04-17T11:15:00+09:00","session_id":"2026-04-17-103000-abc123","skill":"claude-config","phase":"Step 1-2","judgment_type":"value","reference_cited":null,"value_stated":"シンプルな設計の方が望ましい","rationale":"参照文書を引かず直接価値判断を述べた","tool_used":"Grep","context_size":52000}
```

### mixed 判断の例（L6レンズ検出対象）

#### パターンA（reference→value誤写）: `reference_cited` に価値判断語彙が混入

```jsonl
{"timestamp":"2026-04-17T14:00:00+09:00","session_id":"2026-04-17-140000-def456","skill":"harness-tuning","phase":null,"judgment_type":"mixed","reference_cited":"より良いユーザー体験を提供すべき #behaviour-harness","value_stated":null,"rationale":"reference_citedフィールドに価値語彙（より良い、すべき）が混入しており、アンカーIDと価値判断が混在しているためパターンAとして検出","tool_used":"Read","context_size":null}
```

#### パターンB（value→reference誤写）: `value_stated` にアンカーID文字列が混入

```jsonl
{"timestamp":"2026-04-17T14:30:00+09:00","session_id":"2026-04-17-140000-def456","skill":"harness-tuning","phase":"Phase 2","judgment_type":"mixed","reference_cited":null,"value_stated":"#behaviour-harness の基準を満たすべき","rationale":"value_statedフィールドにアンカーID（#behaviour-harness）が混入しており、価値判断の中にreference文字列が紛れ込んでいるためパターンBとして検出","tool_used":"Edit","context_size":61000}
```

## 採用原則

> 「ログは多いほうがよい。余分はわかるが、少ないと足りないことに気付けない」— ユーザー言明（2026-04-16）

- 任意フィールドも可能な限り記録する
- 過少記録は致命的、過剰記録は許容

## L6レンズとの契約

L6レンズ（retrospective）はこのスキーマに基づき以下を消費する:

- `judgment_type == "mixed"` のレコードを混同検出の対象とする
- `reference_cited` と `value_stated` の双方が非nullのレコードを優先的に検査する
- `rationale` から混入パターン（パターンA: reference記述中のvalue語彙混入 / パターンB: value記述中のreference参照混入）を判定する
- 初期閾値: パターンA ≥ 30% / パターンB ≥ 20%（月次調整対象）

## 関連ファイル

- `SKILL.md` — retrospective スキル定義（L6レンズ実装）
- `.claude/tmp/diagnostic-trace/` — ログ出力先ディレクトリ
- `docs/harness-engineering-domain-model.md` — ドメイン目的関数（三項組）の正本
- `.claude/domain-models/2026-04-16-193643-harness-domain-session3.md` — スキーマ設計のモデリング根拠
