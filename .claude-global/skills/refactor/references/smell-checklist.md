# スメル検出チェックリストと深刻度基準

## 深刻度判定基準

| 深刻度 | 条件 | アクション |
|--------|------|----------|
| HIGH | 変更困難 or 重複 | 早期対処推奨 |
| MEDIUM | 可読性低下 | 計画的対処 |
| LOW | 軽微な問題 | 機会があれば対処 |

## スメル検出チェックリスト

Phase 1のスメル検出で使用するチェックリスト:

```
□ メソッドは20行以下か
□ クラスは単一責任か
□ パラメータは3つ以下か
□ 重複コードはないか
□ 使われていないコードはないか
□ 他クラスのデータを過度に使っていないか
□ 変更が複数ファイルに波及しないか
```

## 各スメルの深刻度一覧

| スメル | カテゴリ | 深刻度 |
|--------|---------|--------|
| Long Method | Bloaters | MEDIUM |
| Large Class | Bloaters | HIGH |
| Long Parameter List | Bloaters | MEDIUM |
| Primitive Obsession | Bloaters | MEDIUM |
| Data Clumps | Bloaters | MEDIUM |
| Divergent Change | Change Preventers | HIGH |
| Shotgun Surgery | Change Preventers | HIGH |
| Parallel Inheritance Hierarchies | Change Preventers | MEDIUM |
| Duplicate Code | Dispensables | HIGH |
| Dead Code | Dispensables | LOW |
| Lazy Class | Dispensables | LOW |
| Speculative Generality | Dispensables | MEDIUM |
| Feature Envy | Couplers | MEDIUM |
| Inappropriate Intimacy | Couplers | HIGH |
| Message Chains | Couplers | MEDIUM |

## 4. Couplers（結合度の問題）詳細

### 4.1 Feature Envy
**症状**: メソッドが自クラスより他クラスのデータを多用（他オブジェクトのgetterを多用）
**対処**: **Move Method** — データを持つクラスへメソッドを移動

### 4.2 Inappropriate Intimacy
**症状**: 互いのprivateフィールドにアクセス、双方向の参照、継承の乱用
**対処**: Move Method/Field、Extract Class、Replace Inheritance with Delegation

### 4.3 Message Chains
**症状**: `a.getB().getC().getD().doSomething()` — オブジェクトの内部構造への依存
**対処**: Hide Delegate、Extract Method + Move Method
