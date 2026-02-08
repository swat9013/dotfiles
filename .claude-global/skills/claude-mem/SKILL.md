---
name: claude-mem
description: claude-memプラグインの使用ガイド。MCPツール仕様、3層ワークフロー、Progressive Disclosure、設定オプションを含む。「claude-mem」「メモリ検索」「セッション履歴」「コンテキスト検索」「過去のセッション」と依頼された時に参照する。
user-invocable: false
---

# claude-mem

過去のセッションを検索・参照するためのMCPツール操作ガイド。

## クイックスタート

**鉄則**: search → 絞り込み → get_observations（バッチ指定）

```
1. search("具体的なクエリ")
   ↓ 軽量インデックス取得（~50-100トークン/件 ※公式値）

2. 結果を評価・絞り込み（1-3件が理想）
   ↓ relevance_score 0.7以上を優先

3. get_observations(ids=["id1", "id2"])
   ↓ 詳細データ取得（推定 数千トークン/件）
```

**トークンコスト比較（推定値）:**

| パターン | 相対コスト |
|---------|-----------|
| search → 絞り込み → get（バッチ） | 1x（基準） |
| get_observationsを個別に複数回 | ~2-3x |
| searchスキップで全件取得 | ~5x以上 |

### ワークフロー例

```markdown
1. search("authentication error fix")
   → obs_501〜505 の5件のインデックス

2. インデックスを評価
   - obs_502: "JWT expiration handling" (relevance: 0.85) → 取得
   - obs_504: "session timeout fix" (relevance: 0.72) → 取得
   - obs_501, 503, 505: relevance < 0.5 → スキップ

3. get_observations(ids=["obs_502", "obs_504"])
   → 詳細取得、現在のタスクに適用
```

## MCPツール一覧

| ツール | 用途 | コスト | 使用頻度 |
|--------|------|--------|---------|
| `search` | セマンティック/キーワード検索でID一覧取得 | 低 | 毎回（起点） |
| `timeline` | 指定IDの前後の時系列コンテキスト表示 | 中 | 10-20% |
| `get_observations` | 完全な観測データ取得（**バッチ指定必須**） | 高 | フィルタ後 |
| `save_memory` | 手動でメモリ保存（セマンティックインデックス） | - | 重要情報記録時 |

## 検索ワークフロー

### Layer 1: search — 判断基準

**絞り込み判断:**
- 1-3件: そのまま取得
- 4-5件: relevance_scoreで絞る
- 6件以上: クエリを具体化して再検索

**searchレスポンスの読み方:**

| フィールド | 判断への使い方 |
|-----------|--------------|
| `summary` | 内容の関連性を確認 |
| `relevance_score` | 0.7以上を優先 |
| `timestamp` | 新しいほど関連性高い傾向 |

#### 検索失敗時のリカバリ

```
1. クエリを言い換え: 「React component」→「useState hook refactoring」
2. 抽象化: 「fetchUserData」→「API fetch error handling」
3. 時間範囲を広げる: 「先週の実装」→「認証機能の実装」
4. 別の軸で検索: 機能名 → ファイル名 → エラーメッセージ
```

### Layer 2: timeline — 使用判断

**使う場合（10-20%のケース）:**
- 複数ファイルの変更順序が重要
- デバッグの試行錯誤プロセスを知りたい
- 設定変更後の影響範囲を追跡したい

**スキップする場合:**
- 単一ファイルの変更
- 独立した実装パターン参照
- 最終状態のみ必要

### Layer 3: get_observations — バッチ指定

```python
# ✅ 複数IDを一度に
get_observations(ids=["obs_123", "obs_456", "obs_789"])

# ❌ 個別に複数回（オーバーヘッド増、レイテンシn倍）
get_observations(id="obs_123")
get_observations(id="obs_456")
```

## ベストプラクティス

### クエリ設計

| 検索意図 | クエリ例 | コツ |
|---------|---------|------|
| 前回と同じ方法 | "機能名 method pattern" | 動詞を含める |
| エラー解決 | "error_type fix resolved" | 解決済みを示唆 |
| 設定変更履歴 | "config_file changed update" | ファイル名を含める |
| 実装パターン | "component_name implementation" | 具体的な名前 |
| コーディング規約 | "naming convention style guide" | ルール系キーワード |

**精度向上のポイント:**
- 具体的なファイル名・関数名を含める（"UserProfile.tsx style" > "React style"）
- セマンティック検索なので自然文も有効
- 時間範囲の限定で関連性向上（"先週の" "12月の"）

### プライバシー制御

`<private>` タグで検索対象外に:

```markdown
APIキー: <private>sk-1234567890abcdef</private>

<private>
postgres://user:password@localhost:5432/db
</private>
```

**対象**: 認証情報、個人情報、社内限定ドキュメント引用、本番環境の設定値

### 手動メモリ保存（save_memory）

**推奨タイミング:**
- 重要な設計決定を下した時
- 複雑なバグの解決方法を見つけた時
- プロジェクト固有のパターンを確立した時

```markdown
今回のリファクタリングで学んだこと:
- useCallbackはprops渡し時のみ必要
- カスタムフック化で再利用性向上
```

## Gotchas

### 1. searchスキップで全件取得

**症状**: タイムアウト、巨大レスポンス、トークン大量消費

**対策**: 必ずsearchから開始（3層ワークフローの鉄則）

**見つからない場合**: searchの失敗リカバリフロー（上記）を実行。全件取得は解決策ではない。

### 2. 個別get_observationsのループ

**症状**: トークン消費が想定の2-3倍

**対策**: IDリストを先に作成 → `get_observations(ids=[...])` で一括取得

### 3. 検索クエリが曖昧

**症状**: 無関係な結果が大量に返る

**対策**:
- 「React」→「React useState hook」のように具体化
- ファイル名・関数名・エラーメッセージの一部を含める
- 6件以上返ったら再検索

### 4. プライベートタグ付け忘れ

**症状**: 機密情報が検索結果に出現

**対策**: APIキー・パスワード入力前にタグ付けを習慣化

### 5. timelineの過剰使用

**症状**: 不要なコンテキストでトークン浪費

**対策**: 「前後関係が本当に必要か？」→ NOならスキップ（使用判断は上記参照）

### 6. データベースが空

**症状**: 検索結果が0件（初期セッション）

**対策**: 複数セッション実行後に蓄積される。初回は期待しない。

## 使用パターン

### 基本パターン: search → get_observations

大半のケースはこのフロー。クエリ設計表を参考にクエリを構成:

```markdown
# 目的: 「前回のPRと同じフォーマットで作りたい」
search("pull request creation gh pr create")
→ get_observations(ids=["obs_703"])
→ タイトル形式・本文構造を取得して適用
```

```markdown
# 目的: 「似たようなエラーを前に直したはず」
search("TypeError fix resolved UserProfile")
→ get_observations(ids=["obs_301", "obs_305"])
→ 修正アプローチを現在のバグに適用
```

### 応用パターン: timeline を含むフロー

**前後関係が重要なケース**で使用:

```markdown
# 目的: 「前回のESLint設定変更で何を修正したか」
1. search(".eslintrc config changed") → obs_201
2. timeline(obs_201) → 前後10分: .eslintrc → 5ファイルのlint修正
3. get_observations(ids=["obs_201", ...関連ID]) → 具体的な修正内容
```

```markdown
# 目的: 「前回同じバグをどう調査したか」
1. search("memory leak debugging investigation") → obs_401, obs_403, obs_405
2. timeline(obs_401) → 調査順序: ログ確認 → ブレークポイント → 変数追跡
3. get_observations(ids=["obs_401", "obs_403", "obs_405"]) → 各ステップの詳細
```

## 設定

デフォルト設定で動作。変更はユーザー作業（`~/.claude-mem/settings.json`）。

主要項目: `aiModel`, `workerPort`(37777), `contextInjection.maxResults`(5)

## 成功基準

1. searchで関連IDを特定し、必要最小限のget_observationsで詳細を取得できた
2. 取得した情報を現在のタスクに活用できた

## 完了チェックリスト

- [ ] 3層ワークフロー（search → 絞り込み → get）を遵守した
- [ ] 取得した情報を現在のタスクに活用した
- [ ] プライバシー情報は`<private>`タグで保護した（該当時）
