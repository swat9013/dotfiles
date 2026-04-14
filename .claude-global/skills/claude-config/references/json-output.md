---
name: json-output
description: claude --output-format json のレスポンス構造と日本語テキスト時の jq parse error 対処法
---

# claude --output-format json の使い方

## レスポンス構造

```bash
claude -p "..." --output-format json
```

- **型**: JSON 配列 `[..., {"result": "text"}]`
- **テキスト抽出**: `jq 'last | .result'` で最後の要素の `.result` フィールドを取得

## 日本語テキスト時の jq parse error 問題

日本語プロンプト時に Claude レスポンスに制御文字（未エスケープ改行等）が含まれ、jq で parse error が発生することがある。

### 対策

| 方法 | コマンド | 備考 |
|------|---------|------|
| 末尾改行削除 | `printf '%s' "$raw"` | `echo "$raw"` を置き換え |
| 文字列化してパース | `jq -Rs '.'` | 入力を一度文字列化して制御文字を自動エスケープ |
| Python JSON パーサー | `python3 -c "import json,sys; ..."` | 制御文字に最も堅牢 |

**How to apply:** `claude --output-format json` を使うシェルスクリプト（wt-switch 等）で jq parse error が出た場合は、まず制御文字エスケープの問題を疑う。
