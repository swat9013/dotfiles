# アンチパターン一覧

## 1. データリーケージ（最重要）

| パターン | 症状 | 対策 |
|---------|------|------|
| 分割前に `fit_transform` | テスト統計量が訓練に混入 | 分割ファースト原則。`Pipeline` でステップ化 |
| 全データで特徴選択 | テスト信号で特徴を選択 | `Pipeline` 内に `SelectKBest` を配置 |
| 時系列で全体前方補間 | 未来データが訓練に混入 | 時間ベース分割 (`TimeSeriesSplit`)、各期間で独立処理 |

## 2. pandasの非効率

| パターン | 速度低下 | 代替手法 |
|---------|---------|---------|
| `iterrows()` | 100–1000倍遅い | ベクトル化操作 |
| `apply(lambda)` | 2–500倍遅い | ベクトル化 or `np.select()` |
| Chained Indexing (`df['a']['b']`) | バグ誘発（SettingWithCopy） | `.loc[condition, 'column']` |
| Sequential Mutation（逐次代入） | 可読性低下 | Method Chaining（`.pipe()`, `.assign()`） |
| インデックス不整合 | 行の重複・欠落 | `.reset_index(drop=True)` |

## 3. スケーリング・エンコーディング

| パターン | 問題 | 対策 |
|---------|------|------|
| テスト集合で `fit` | データリーケージ | 訓練で `fit` → テストは `transform` のみ |
| 順序なしカテゴリに `LabelEncoder` | 偽の順序関係を学習 | `OneHotEncoder` or `TargetEncoder` |
| 未知カテゴリ未対応 | 本番で `KeyError` | `handle_unknown='ignore'` or `'infrequent_if_exist'` |
| スケーラーインスタンス不一致 | 異なる統計量で変換 | 単一インスタンスを `fit` → 同一インスタンスで `transform` |

## 4. 欠損値処理

| パターン | 問題 | 対策 |
|---------|------|------|
| 無思考な `dropna()` | MAR/MNAR でバイアス発生 | 欠損メカニズム判定 → 適切な補完 |
| 無差別な平均値補完 | 分布変化、リーケージリスク | `KNNImputer`, `IterativeImputer` |
| MCAR/MAR/MNAR 無視 | 補完値が実態と乖離 | `missingno` 可視化 → メカニズム判定 |

## 5. メモリ関連

| パターン | 問題 | 対策 |
|---------|------|------|
| デフォルト dtype 放置 | 最大8倍メモリ浪費 | `read_csv` 時に `dtype` 辞書指定 |
| 不要な `df.copy()` | メモリ倍増 | pandas 3.0 CoW 活用 |
| チャンキング忘れ | 数十GB でOOM | `chunksize` 使用 |

## 6. 再現性破壊

| パターン | 問題 | 対策 |
|---------|------|------|
| `random_state` 未設定 | 実行ごとに結果変動 | 全箇所にシード明示 |
| DBクエリ順序不定 | データ順序非決定的 | `ORDER BY` 付与 |
| 環境間シード不整合 | numpy/torch/cuda で結果不一致 | 包括的 `set_seed()` 関数で一括設定 |

## 7. コード品質

| パターン | 問題 | 対策 |
|---------|------|------|
| マジックナンバー | 意味不明な閾値 | `PreprocessingConfig` 等で設定一元管理 |
| 前処理ロジック散在 | 一貫性不明、保守困難 | `Preprocessor` クラス or `Pipeline` 統合 |
| テスト不在 | 本番でバグ発覚 | `pytest` + `Pandera` でデータ品質テスト |
