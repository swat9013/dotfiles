# Claude API Tool Search Tool

Claude APIがツールをオンデマンドで動的に検索・読み込みするための機能。

## 概要

Tool Search Toolは、大量のツール（10個以上）を扱う際のコンテキスト効率化とツール選択精度向上のための機能。

### 主な利点

- **コンテキスト効率化**: 50ツール≈10-20Kトークンが不要になる
- **ツール選択精度向上**: 30〜50個以上のツールでも精度を維持
- **スケーラビリティ**: 最大10,000ツールに対応

## 2つのバリアント

| バリアント | 型名 | 検索方法 |
|-----------|------|----------|
| Regex | `tool_search_tool_regex_20251119` | Python正規表現パターン |
| BM25 | `tool_search_tool_bm25_20251119` | 自然言語クエリ |

### Regexクエリ例

```
"weather"           # "weather"を含むツール
"get_.*_data"       # get_user_data, get_weather_data等
"(?i)slack"         # 大文字小文字を区別しない
```

## 基本的な使用方法

### Python

```python
import anthropic

client = anthropic.Anthropic()

response = client.beta.messages.create(
    model="claude-sonnet-4-5-20250929",
    betas=["advanced-tool-use-2025-11-20"],
    max_tokens=2048,
    messages=[
        {"role": "user", "content": "What is the weather in Tokyo?"}
    ],
    tools=[
        {
            "type": "tool_search_tool_regex_20251119",
            "name": "tool_search_tool_regex"
        },
        {
            "name": "get_weather",
            "description": "Get weather at a specific location",
            "input_schema": {
                "type": "object",
                "properties": {
                    "location": {"type": "string"},
                    "unit": {"type": "string", "enum": ["celsius", "fahrenheit"]}
                },
                "required": ["location"]
            },
            "defer_loading": True  # 遅延ロード設定
        }
    ]
)
```

### cURL

```bash
curl https://api.anthropic.com/v1/messages \
    --header "x-api-key: $ANTHROPIC_API_KEY" \
    --header "anthropic-version: 2023-06-01" \
    --header "anthropic-beta: advanced-tool-use-2025-11-20" \
    --header "content-type: application/json" \
    --data '{
        "model": "claude-sonnet-4-5-20250929",
        "max_tokens": 2048,
        "messages": [{"role": "user", "content": "天気を教えて"}],
        "tools": [
            {"type": "tool_search_tool_regex_20251119", "name": "tool_search_tool_regex"},
            {
                "name": "get_weather",
                "description": "Get weather",
                "input_schema": {"type": "object", "properties": {"location": {"type": "string"}}},
                "defer_loading": true
            }
        ]
    }'
```

## Deferred Tool Loading（遅延ロード）

`defer_loading: true` を指定したツールは、検索で発見された時のみロードされる。

```json
{
  "name": "get_weather",
  "description": "Get current weather for a location",
  "input_schema": { ... },
  "defer_loading": true
}
```

### 重要なポイント

- ツール検索ツール自体には `defer_loading` をつけない
- 最頻使用の3〜5ツールは非遅延にする（パフォーマンス最適化）
- 全ツールを遅延ロードにするとエラーになる

## プロバイダー別Beta Header

| プロバイダー | Beta Header | 対応モデル |
|-------------|-------------|-----------|
| Claude API / Microsoft Foundry | `advanced-tool-use-2025-11-20` | Opus 4.5, Sonnet 4.5 |
| Google Cloud Vertex AI | `tool-search-tool-2025-10-19` | Opus 4.5, Sonnet 4.5 |
| Amazon Bedrock | `tool-search-tool-2025-10-19` | Opus 4.5 |

## ワークフロー

1. Tool search toolと遅延ロードツールを定義
2. Claudeがユーザーの質問を受ける
3. Claudeがツール検索ツールを使用
4. APIが3〜5個の関連ツールを返す（tool_reference形式）
5. 参照が自動的に完全なツール定義に展開
6. Claudeが発見したツールを選択・実行

## MCP統合

```python
client.beta.messages.create(
    model="claude-sonnet-4-5-20250929",
    betas=["advanced-tool-use-2025-11-20", "mcp-client-2025-11-20"],
    max_tokens=2048,
    mcp_servers=[
        {"type": "url", "name": "db-server", "url": "https://mcp-db.example.com"}
    ],
    tools=[
        {"type": "tool_search_tool_regex_20251119", "name": "tool_search_tool_regex"},
        {
            "type": "mcp_toolset",
            "mcp_server_name": "db-server",
            "default_config": {"defer_loading": True},
            "configs": {
                "search_events": {"defer_loading": False}  # このツールは即座にロード
            }
        }
    ],
    messages=[{"role": "user", "content": "データベースのイベントを検索して"}]
)
```

## 制限事項

| 項目 | 制限 |
|------|------|
| 最大ツール数 | 10,000個 |
| 検索結果数 | 1回あたり3〜5個 |
| 正規表現最大長 | 200文字 |
| 対応モデル | Sonnet 4.0以上、Opus 4.0以上（Haikuは非対応） |

## エラーコード

| コード | 説明 |
|--------|------|
| `too_many_requests` | レート制限超過 |
| `invalid_pattern` | 正規表現の不正な形式 |
| `pattern_too_long` | パターンが200文字超過 |
| `unavailable` | サービス一時利用不可 |

## 使用推奨ケース

### 適している場合

- 10個以上のツール
- ツール定義が10K以上のトークン消費
- ツール選択精度に課題がある
- MCPで200以上のツール
- ツールライブラリが成長中

### 従来方式が適している場合

- ツール総数が10個未満
- すべてのツールが毎回使用される
- ツール定義が非常に小さい（合計100トークン未満）

## 最適化のヒント

1. 最頻使用の3〜5ツールは非遅延にする
2. 明確で説明的なツール名と説明を使用
3. セマンティックキーワードを説明に含める
4. システムプロンプトで利用可能なツール分類を説明
5. Claudeが発見したツールを監視して説明を改善

## 使用状況追跡

```json
{
  "usage": {
    "input_tokens": 1024,
    "output_tokens": 256,
    "server_tool_use": {
      "tool_search_requests": 2
    }
  }
}
```

## 参考資料

- [Tool search tool公式ドキュメント](https://docs.anthropic.com/en/docs/agents-and-tools/tool-use/tool-search-tool)
- [Anthropic Cookbook](https://github.com/anthropics/anthropic-cookbook)
- [Claude API Reference](https://docs.anthropic.com/en/api)
