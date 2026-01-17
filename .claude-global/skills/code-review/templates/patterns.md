# コードレビュー: よくあるパターンと修正例

## N+1クエリ

```typescript
// NG: ループ内でクエリ
for (const user of users) { await user.getPosts(); }
// OK: Eager Loading
const users = await User.findAll({ include: [Post] });
```

## エラーハンドリング

```typescript
// NG: レスポンスチェックなし
return response.json();
// OK: ステータス確認
if (!response.ok) throw new Error(`HTTP ${response.status}`);
```

## 依存注入・VO・リソース分離

```typescript
// NG: 依存を直接生成
class UserService { private db = new Database(); }
// OK: コンストラクタで注入
class UserService { constructor(private db: Database) {} }

// NG: プリミティブ型のまま: amount: number; email: string;
// OK: VOとして抽出: Money, Email（バリデーション内包）

// NG: 混在: status + statusHistory[] が同じエンティティ
// OK: 分離: Order（現在状態）+ OrderStatusChanged（履歴イベント）
```

## テスト: Mock/Stub・実装詳細の検証

```typescript
// NG: Stubの呼び出しを検証（実装詳細への結合）
verify(userRepo.findById).toHaveBeenCalledWith(id);

// NG: 内部メソッドをspy（リファクタリングで壊れる）
expect(jest.spyOn(service, 'calculateDiscount')).toHaveBeenCalled();

// OK: 最終結果のみを検証
expect(result).toEqual(expectedUser);
expect(result.total).toBe(900);
```

## コメント

```typescript
// NG: Howの説明: x = x + 1; // xに1を足す
// NG: 命名で解決可能: const d = 7; // 保持日数
// OK: 命名で表現: const retentionDays = 7;
// OK: Why not: // ループで実装（再帰はスタックオーバーフローの恐れ）
```

## 設計判断（トレードオフ）の提示

```ruby
# 一般的なパターン（1クエリ）
titles = Title.bookmarked_by(user_id).current

# 意図的な分離（2クエリ）- SQLオプティマイザー問題回避のため
bookmarked_ids = Title.bookmarked_by(user_id).pluck(:id)
current_ids = Title.where(id: bookmarked_ids).current.pluck(:id)
```

レビュー時の対応:
- MR説明に分離理由あり → 現状維持推奨 + 統合案を選択肢として提示
- 理由が不明 → 統合の選択肢を提示し意図を確認

## 複雑なクエリの実行計画

複雑なクエリ（OR結合、EXISTSサブクエリ等）は、SQLオプティマイザーが期待通りの実行計画を選択しない場合がある。

```ruby
# 注意: 複雑なクエリは実行計画の確認が必要
titles = Title.where(...).or(Title.where(...))
# → 本番相当データでEXPLAIN ANALYZEを確認すること
```

**確認が必要なケース**:
- スコープが `or()` を使用している
- スコープ内に `EXISTS` サブクエリがある
- 大量データを扱う可能性がある

**対策**:
- 本番相当データで `EXPLAIN ANALYZE` を確認し、期待通りの実行計画か検証
- 問題がある場合は、クエリ分離やインデックス追加など個別に対応を検討
