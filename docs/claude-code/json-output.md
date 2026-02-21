# JSON出力仕様

## 概要

`claude -p`（print mode）でJSON形式の出力を取得する方法。

## 基本フラグ

| フラグ | 説明 |
|--------|------|
| `--output-format json` | JSON形式で出力（NDJSON: 1行1オブジェクト） |
| `--output-format stream-json` | ストリーミングJSON出力 |
| `--json-schema '{...}'` | 構造化出力を強制（スキーマに一致するJSONを返す） |

## 出力構造

`--output-format json` の出力は複数のJSONオブジェクトで構成される:

```
{"type":"system","subtype":"hook_started",...}
{"type":"system","subtype":"hook_response",...}
{"type":"assistant","message":{"content":[...]},...}
{"type":"result","subtype":"success","structured_output":{...},...}
```

### 主要なtype

| type | 説明 |
|------|------|
| `system` | hook情報、セッション情報 |
| `assistant` | Claudeの応答テキスト |
| `result` | 最終結果（structured_outputを含む） |

## 構造化出力（--json-schema）

余分なテキストなしで、指定したスキーマに一致するJSONを取得できる。

### 使用例

```bash
SCHEMA='{"type":"object","properties":{"message":{"type":"string"}},"required":["message"]}'

claude --model haiku -p --output-format json --json-schema "$SCHEMA" \
  "Generate a greeting" 2>/dev/null | \
  jq -s -r '.[] | select(.type=="result") | .structured_output.message'
```

### 出力の抽出パターン

```bash
# テキスト出力（従来）- 余計なメッセージが混入する可能性あり
claude -p "query"

# 構造化出力 - スキーマで指定したフィールドのみ
claude -p --output-format json --json-schema "$SCHEMA" "query" | \
  jq -r '.[] | select(.type=="result") | .structured_output.FIELD'
```

## 実用例: git aicommit

コミットメッセージのみを確実に取得:

```bash
#!/bin/bash
SCHEMA='{"type":"object","properties":{"message":{"type":"string"}},"required":["message"]}'

COMMITMSG=$(claude --model haiku -p --output-format json --json-schema "$SCHEMA" \
  'Based on `git diff --cached`, generate a commit message...' 2>/dev/null | \
  jq -s -r '.[] | select(.type=="result") | .structured_output.message')

git commit -m "$COMMITMSG" -e
```

## エラーハンドリング

result の subtype でエラーを判定:

| subtype | 説明 |
|---------|------|
| `success` | 正常終了 |
| `error_max_structured_output_retries` | スキーマに一致する出力を生成できなかった |

```bash
# エラーチェック
RESULT=$(claude -p --output-format json --json-schema "$SCHEMA" "query" | \
  jq -s '[.[] | select(.type=="result")] | .[0]')

if [[ $(echo "$RESULT" | jq -r '.subtype') == "success" ]]; then
  echo "$RESULT" | jq -r '.structured_output.message'
else
  echo "Error: Failed to generate output" >&2
fi
```

## 参考

- [CLI Reference](https://code.claude.com/docs/en/cli-reference)
- [Agent SDK Structured Outputs](https://platform.claude.com/docs/en/agent-sdk/structured-outputs)
