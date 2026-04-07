---
name: python-data-preprocessing
user-invocable: false
description: |-
  Pythonデータ前処理のツール選択・パイプライン設計・アンチパターン回避の判断基準を提供。
  Use when「データ前処理」「pandas」「Polars」「scikit-learn Pipeline」「データリーケージ」。
---

# Python Data Preprocessing

## 1. ツール選択マトリクス

| データサイズ | 用途 | 推奨ツール | パターン |
|-------------|------|-----------|---------|
| 〜1GB | ML訓練 | pandas + scikit-learn Pipeline | A |
| 〜1GB | ETL/分析 | pandas（DuckDB も選択肢） | A |
| 1〜10GB | パフォーマンス重視 | Polars + DuckDB | B |
| 1〜10GB | 既存pandasコード大 | pandas + dtype最適化 + chunksize | A改 |
| 10GB+ シングルマシン | ETL | DuckDB + Polars | B |
| 10GB+ クラスタあり | ETL | Dask + pandas | C |
| 任意 | SQL記述が主 | DuckDB | B |
| 任意 | マルチバックエンドライブラリ開発 | narwhals（ニッチ） | — |

### ワークフローパターン

- **A（小〜中規模ML）**: pandas → scikit-learn Pipeline → fit/predict
- **B（中〜大規模ETL）**: DuckDB（SQL変換） → Polars（高速処理） → pandas（最終加工）
- **C（エンタープライズ）**: Dask + pandas or PySpark

## 2. 判断フロー

```
ツール選択フロー
├─ データ 1GB 以下？
│   ├─ ML モデル訓練 → pandas + scikit-learn Pipeline
│   └─ ETL/分析 → pandas（DuckDB も選択肢）
├─ データ 1-10GB？
│   ├─ パフォーマンスクリティカル → Polars（+ DuckDB for SQL）
│   └─ 既存 pandas コードベース大 → pandas + dtype最適化 + チャンク
├─ データ 10GB+ + クラスタ環境あり → Dask
├─ データ 10GB+ + シングルマシン → DuckDB + Polars
├─ SQL 記述が主 → DuckDB
└─ ライブラリ開発でマルチバックエンド → narwhals
    └─ 詳細 → references/tool-comparison.md
```

## 3. パイプライン設計原則

| # | 原則 | 要点 |
|---|------|------|
| 1 | 分割ファースト | `train_test_split` を最初に。全データで `fit_transform` 禁止 |
| 2 | Fit-Transform分離 | 訓練でのみ `fit`。テスト/本番は `transform` のみ呼ぶ |
| 3 | シード一元管理 | `random`, `numpy`, `torch`, CUDA の全乱数源を1関数でまとめて設定 |
| 4 | Pipeline統合 | scikit-learn `Pipeline` / `ColumnTransformer` で前処理→学習を一体化 |
| 5 | 型安全2層検証 | Pydantic（行レベル）+ Pandera（テーブルレベル）の多層防御 |
| 6 | メモリ意識 | dtype最適化（int64→int8で87.5%削減）、`usecols`、`chunksize` |

## 4. 最重要 Gotchas

### データリーケージ（症状→原因→対策）

| パターン | 症状 | 原因 | 対策 |
|---------|------|------|------|
| 分割前 fit_transform | 本番スコアが訓練より大幅低下 | テストデータの分布情報が訓練に流入 | 分割後に fit_transform |
| 全データ特徴選択 | 特徴選択後にデータ分割 → 評価過楽観 | 選択基準にテスト情報混入 | 分割→特徴選択の順序厳守 |
| 時系列前方補間 | 将来データを過去補間に使用 → リーク | `fillna` が未来値を参照 | 時系列は `ffill` のみ（後方参照禁止） |

### pandas非効率（症状→原因→対策）

| パターン | 速度劣化 | 原因 | 対策 |
|---------|---------|------|------|
| `iterrows` | 100〜1000倍遅 | Python ループで行ごとにオブジェクト生成 | `vectorize` / `apply` with `np` |
| `apply` 乱用 | 2〜500倍遅 | Python関数呼び出しオーバーヘッド | ベクトル演算 / Polars に移行 |
| Chained Indexing | 意図せぬ `SettingWithCopy` | コピー vs ビューの不確定動作 | `.loc[row, col]` で一括指定 |

### 再現性破壊（症状→原因→対策）

| パターン | 症状 | 原因 | 対策 |
|---------|------|------|------|
| `random_state` 未設定 | 実行ごとに結果が変わる | 乱数源が初期化されない | 全乱数源を seed 関数で統一初期化 |
| DBクエリ順序不定 | 同一クエリで行順変動 | ORDER BY なし→DBが任意順を返す | 必ず `ORDER BY` を明示 |
| 環境間シード不整合 | ローカルと CI で結果が異なる | `PYTHONHASHSEED` 等の環境差異 | `.env` / CI設定で全環境統一 |

## 5. references/ ナビゲーション

「何を調べたいか → どのファイルを読むか」対応表:

| 調査目的 | 参照先 |
|---------|-------|
| ツール詳細比較・トレードオフ（Polars vs pandas API差、DuckDB制約など） | `references/tool-comparison.md` |
| アンチパターン全量・チェックリスト（リーケージ・非効率・再現性） | `references/antipatterns.md` |
| 欠損値処理・メモリ最適化・検証パターン（Pydantic/Pandera設定例） | `references/pipeline-patterns.md` |
