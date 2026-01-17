# コードレビュー: よくあるパターンと修正例

## Security

### インジェクション

```typescript
// NG: 文字列結合でクエリ構築
const query = `SELECT * FROM users WHERE id = ${userId}`;
// OK: パラメータバインディング
const query = 'SELECT * FROM users WHERE id = ?';
db.query(query, [userId]);
```

### 認証情報のハードコード

```typescript
// NG: ハードコード
const apiKey = 'sk-1234567890';
// OK: 環境変数
const apiKey = process.env.API_KEY;
```

## Correctness

### N+1クエリ

```typescript
// NG: ループ内でクエリ
for (const user of users) { await user.getPosts(); }
// OK: Eager Loading
const users = await User.findAll({ include: [Post] });
```

### 競合状態

```typescript
// NG: check-then-act
if (await cache.exists(key)) {
  return await cache.get(key);  // 別スレッドで削除される可能性
}
// OK: アトミック操作
return await cache.getOrSet(key, computeValue);
```

### エラーハンドリング

```typescript
// NG: レスポンスチェックなし
return response.json();
// OK: ステータス確認
if (!response.ok) throw new Error(`HTTP ${response.status}`);
```

### リソースリーク

```typescript
// NG: クローズ漏れ
const conn = await pool.getConnection();
const result = await conn.query(sql);
return result;
// OK: finally で確実にクローズ
const conn = await pool.getConnection();
try {
  return await conn.query(sql);
} finally {
  conn.release();
}
```

## Design

### 依存注入

```typescript
// NG: 依存を直接生成
class UserService { private db = new Database(); }
// OK: コンストラクタで注入
class UserService { constructor(private db: Database) {} }
```

### バリューオブジェクト

```typescript
// NG: プリミティブ型のまま
amount: number;
email: string;
// OK: VOとして抽出（バリデーション内包）
amount: Money;
email: Email;
```

### リソースとイベントの分離

```typescript
// NG: 混在: status + statusHistory[] が同じエンティティ
// OK: 分離: Order（現在状態）+ OrderStatusChanged（履歴イベント）
```

### 複雑なクエリの実行計画

複雑なクエリ（OR結合、EXISTSサブクエリ等）は、SQLオプティマイザーが期待通りの実行計画を選択しない場合がある。

**確認が必要なケース**:
- `or()` を使用
- `EXISTS` サブクエリがある
- 大量データを扱う可能性がある

**対策**: 本番相当データで `EXPLAIN ANALYZE` を確認

## Testing

### Mock/Stubの誤用

```typescript
// NG: Stubの呼び出しを検証（実装詳細への結合）
verify(userRepo.findById).toHaveBeenCalledWith(id);

// NG: 内部メソッドをspy（リファクタリングで壊れる）
expect(jest.spyOn(service, 'calculateDiscount')).toHaveBeenCalled();

// OK: 最終結果のみを検証
expect(result).toEqual(expectedUser);
expect(result.total).toBe(900);
```

### テストの独立性

```typescript
// NG: 他のテストの副作用に依存
test('should update user', async () => {
  // 前のテストで作成されたユーザーを期待
  await updateUser(1, { name: 'new' });
});

// OK: 各テストで独立したセットアップ
test('should update user', async () => {
  const user = await createTestUser();
  await updateUser(user.id, { name: 'new' });
});
```

## コメント

```typescript
// NG: Howの説明: x = x + 1; // xに1を足す
// NG: 命名で解決可能: const d = 7; // 保持日数
// OK: 命名で表現: const retentionDays = 7;
// OK: Why not: // ループで実装（再帰はスタックオーバーフローの恐れ）
```
