# コードスメルカタログ

リファクタリングのトリガーとなるコードスメル15種と対処法。

## Contents
- [1. Bloaters（肥大化）](#1-bloaters肥大化)
- [2. Change Preventers（変更の障害）](#2-change-preventers変更の障害)
- [3. Dispensables（不要な要素）](#3-dispensables不要な要素)
- [4. Couplers（結合度の問題）](#4-couplers結合度の問題)
- [深刻度判定基準](#深刻度判定基準)
- [スメル検出チェックリスト](#スメル検出チェックリスト)

## 1. Bloaters（肥大化）

コードが過度に大きくなり、扱いにくくなっている状態。

### 1.1 Long Method

**症状**: メソッドが長すぎる（目安: 20行超）

**兆候**:
- スクロールしないと全体が見えない
- 複数の抽象レベルが混在
- コメントで区切られたブロックがある

**対処法**:
- **Extract Method**: コードブロックを新メソッドに抽出
- **Replace Temp with Query**: 一時変数をメソッド呼び出しに置換

**深刻度**: MEDIUM

---

### 1.2 Large Class

**症状**: クラスが多くの責務を持ちすぎている

**兆候**:
- フィールドが10個以上
- メソッドが多すぎる
- 関連性の低いメソッド群がある

**対処法**:
- **Extract Class**: 責務を分離して新クラス作成
- **Extract Subclass**: サブクラスとして分離

**深刻度**: HIGH

---

### 1.3 Long Parameter List

**症状**: パラメータが多すぎる（目安: 4つ以上）

**兆候**:
- 呼び出し側で引数の順序を間違えやすい
- 似たようなパラメータが複数メソッドに出現

**対処法**:
- **Introduce Parameter Object**: パラメータをオブジェクトにまとめる
- **Preserve Whole Object**: オブジェクト全体を渡す

**深刻度**: MEDIUM

---

### 1.4 Primitive Obsession

**症状**: プリミティブ型への過度な依存

**兆候**:
- 文字列で複雑なデータを表現
- 数値に特別な意味がある（マジックナンバー）
- 配列で異種データを扱う

**対処法**:
- **Replace Data Value with Object**: 値オブジェクトを導入
- **Replace Type Code with Class**: 型コードをクラスに置換

**深刻度**: MEDIUM

---

### 1.5 Data Clumps

**症状**: 常に一緒に現れるデータの群れ

**兆候**:
- 同じ3つ以上のフィールドが複数クラスに出現
- 同じパラメータの組み合わせが複数メソッドに出現

**対処法**:
- **Extract Class**: データ群をクラスにまとめる
- **Introduce Parameter Object**: パラメータ群をオブジェクト化

**深刻度**: MEDIUM

---

## 2. Change Preventers（変更の障害）

変更が困難になる構造的問題。

### 2.1 Divergent Change

**症状**: 1つのクラスが複数の理由で変更される

**兆候**:
- 「機能Aを変更するときはこのメソッド群」「機能Bはこっち」
- 異なる関心事が1クラスに混在

**対処法**:
- **Extract Class**: 各関心事を別クラスに分離

**深刻度**: HIGH

---

### 2.2 Shotgun Surgery

**症状**: 1つの変更が複数クラスに波及

**兆候**:
- 小さな変更で多くのファイルを編集
- 同じ変更パターンが散在

**対処法**:
- **Move Method/Field**: 関連する機能を1箇所に集約
- **Inline Class**: 過度に分散したクラスを統合

**深刻度**: HIGH

---

### 2.3 Parallel Inheritance Hierarchies

**症状**: 継承階層が並列化している

**兆候**:
- クラスAにサブクラスを追加すると、クラスBにも対応するサブクラスが必要

**対処法**:
- **Move Method/Field**: 一方の階層を他方に統合

**深刻度**: MEDIUM

---

## 3. Dispensables（不要な要素）

削除しても問題ない、または削除すべき要素。

| スメル | 症状 | 兆候 | 対処法 | 深刻度 |
|--------|------|------|--------|--------|
| 3.1 Duplicate Code | 類似コードが複数箇所 | コピペ、類似メソッド | Extract Method, Pull Up Method, Form Template Method | HIGH |
| 3.2 Dead Code | 使われていないコード | 到達不能ブロック、呼ばれないメソッド | 削除（バージョン管理で復元可能） | LOW |
| 3.3 Lazy Class | 存在価値の薄いクラス | メソッド1-2個、委譲のみ | Inline Class, Collapse Hierarchy | LOW |
| 3.4 Speculative Generality | 「将来使うかも」の汎用化 | 使われない抽象クラス・不要パラメータ | Collapse Hierarchy, Inline Class/Method | MEDIUM |

---

## 4. Couplers（結合度の問題）

詳細は `smell-checklist.md` を参照。

---

## 深刻度判定基準・スメル検出チェックリスト

詳細は `smell-checklist.md` を参照。
