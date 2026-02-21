#!/bin/bash
# セッションJSONLからuser/assistant/tool_resultメッセージを抽出
# 引数: JSONLファイルパス
# 出力: 整形されたメッセージ（Markdown形式）

set -euo pipefail

JSONL_FILE="${1:-}"

if [[ -z "$JSONL_FILE" ]]; then
  echo "Usage: $0 <session.jsonl>" >&2
  exit 1
fi

if [[ ! -f "$JSONL_FILE" ]]; then
  echo "Error: File not found: $JSONL_FILE" >&2
  exit 1
fi

# jqフィルター: user/assistant/tool_resultのみ抽出し、メッセージを整形
# 構造:
#   user/assistant: .message.content が string または array
#   tool_result: .content が string または array（.messageラッパーなし）
jq -r '
  select(.type == "user" or .type == "assistant" or .type == "tool_result") |
  .type as $type |

  (
    if $type == "tool_result" then
      .content as $content |
      if $content | type == "string" then
        $content
      elif $content | type == "array" then
        [$content[] |
          if .type == "text" then .text
          else null
          end
        ] | map(select(. != null and . != "")) | join("\n")
      else
        ""
      end
    else
      .message.content as $content |
      if $content | type == "string" then
        $content
      elif $content | type == "array" then
        [$content[] |
          if .type == "text" then .text
          elif .type == "tool_use" then "【tool: \(.name)】"
          else null
          end
        ] | map(select(. != null and . != "")) | join("\n")
      else
        ""
      end
    end
  ) as $text |

  select($text != "") |
  "## \($type | ascii_upcase)\n\($text)\n"
' "$JSONL_FILE" 2>/dev/null || {
  echo "Error: Failed to parse JSONL" >&2
  exit 1
}
