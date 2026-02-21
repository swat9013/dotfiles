# Hooks 設計ガイド

## 概要

Hooksはツール実行やセッションのライフサイクルイベントに応じてシェルコマンドを自動実行する仕組み。ファイル編集後のlinter実行、機密ファイルへのアクセスブロック、セッション開始時のコンテキスト注入などに活用できる。

## ファイル配置

| パス | 用途 | 優先度 |
|-----|------|-------|
| `.claude/settings.json` | プロジェクト設定（チーム共有） | 高 |
| `.claude/settings.local.json` | ローカル設定（gitignore） | 高 |
| `~/.claude/settings.json` | ユーザー設定（全プロジェクト） | 低 |

## イベント一覧

| イベント | 発火タイミング | ブロック可否 | 主な用途 |
|---------|---------------|------------|---------|
| `SessionStart` | セッション開始・再開時 | 不可 | コンテキスト注入、環境設定 |
| `UserPromptSubmit` | プロンプト送信時 | 可 | 入力検証、追加コンテキスト |
| `PreToolUse` | ツール実行前 | 可 | コマンドブロック、入力検証 |
| `PermissionRequest` | 許可ダイアログ表示時 | 可 | 自動承認/拒否 |
| `PostToolUse` | ツール成功後 | 不可（フィードバックのみ） | **linter/formatter実行** |
| `PostToolUseFailure` | ツール失敗後 | 不可（フィードバックのみ） | エラーハンドリング |
| `Stop` | Claude応答完了時 | 可 | 品質ゲート、タスク検証 |
| `SubagentStart` | サブエージェント起動時 | 不可 | コンテキスト注入（2025新機能） |
| `SubagentStop` | サブエージェント完了時 | 可 | 出力品質検証 |
| `TeammateIdle` | チームメートがアイドル化直前 | 可 | マルチエージェント品質ゲート（2025新機能） |
| `TaskCompleted` | タスク完了マーク時 | 可 | 完了前品質検証（2025新機能） |
| `PreCompact` | コンパクション前 | 不可 | 状態保存 |
| `SessionEnd` | セッション終了時 | 不可 | クリーンアップ、ログ |
| `Notification` | 通知送信時 | 不可 | カスタム通知 |
| `ConfigChange` | 設定ファイル変更時 | 可（policy_settings除く） | 監査ログ（2025新機能） |

## Exit Code規則

| コード | 意味 | 動作 |
|-------|------|------|
| `0` | 成功 | stdout表示（verbose）、JSON解析あり |
| `2` | ブロッキングエラー | stderrがClaudeにフィードバック |
| その他 | 非ブロッキングエラー | stderr表示、実行継続 |

## 環境変数

| 変数 | 説明 |
|------|------|
| `CLAUDE_PROJECT_DIR` | プロジェクトルートの絶対パス |
| `CLAUDE_ENV_FILE` | SessionStart用、環境変数永続化ファイル |
| `CLAUDE_CODE_REMOTE` | リモート環境なら`"true"` |
| `CLAUDE_PLUGIN_ROOT` | プラグインディレクトリパス |

## 基本構文

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "your-command-here",
            "timeout": 30
          }
        ]
      }
    ]
  }
}
```

### matcher パターン

- `Write|Edit`: WriteまたはEditツール
- `Bash`: Bashツールのみ
- 省略: 全ツールにマッチ
- `mcp__<server>__<tool>`: MCPツール（例: `mcp__github__create_issue`）

### hook type 種類

| type | 説明 | デフォルトtimeout | 用途 |
|------|------|----------------|------|
| `command` | シェルコマンド実行 | 600秒 | フォーマット、検証、ログ |
| `prompt` | LLM（Haiku）による単一ターン評価（2025新機能） | 30秒 | 自然言語でのルール判定 |
| `agent` | マルチターンサブエージェント・最大50ターン（2025新機能） | 60秒 | ファイル検査、テスト実行 |

`command` hookの追加フィールド:
- `async: true` — バックグラウンド実行。Claudeをブロックせず非同期実行（2026-01追加）
- `statusMessage` — フック実行中のスピナーメッセージ

`prompt` hook のレスポンス形式（LLMがstdoutにJSONを返す）:

```json
{ "ok": true }
{ "ok": false, "reason": "続行できない理由" }
```

## 実装パターン

### 編集後linter実行（推奨）

jqでstdinからファイルパスを抽出する方式が最も確実:

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "jq -r '.tool_input.file_path' | xargs -I {} sh -c 'prettier --write \"{}\"'"
          }
        ]
      }
    ]
  }
}
```

### 拡張子別の処理

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "jq -r '.tool_input.file_path' | { read f; [[ \"$f\" == *.ts ]] && npx prettier --write \"$f\" || true; }"
          }
        ]
      }
    ]
  }
}
```

### 外部スクリプト化（複雑な処理向け）

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "\"$CLAUDE_PROJECT_DIR\"/.claude/hooks/format-and-lint.sh"
          }
        ]
      }
    ]
  }
}
```

`.claude/hooks/format-and-lint.sh`:

```bash
#!/bin/bash
FILE_PATH=$(jq -r '.tool_input.file_path')

case "$FILE_PATH" in
  *.ts|*.tsx|*.js|*.jsx)
    npx prettier --write "$FILE_PATH" 2>/dev/null
    npx eslint --fix "$FILE_PATH" 2>/dev/null
    ;;
  *.py)
    ruff format "$FILE_PATH" 2>/dev/null
    ruff check --fix "$FILE_PATH" 2>/dev/null
    ;;
esac

exit 0
```

### 機密ファイルのブロック

PreToolUse のブロックには `hookSpecificOutput` 形式（推奨）と `exit 2 + stderr` の2方式がある:

```bash
#!/bin/bash
# ~/.claude/hooks/protect-sensitive.sh
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

# パストラバーサルチェック
if [[ "$FILE_PATH" == *".."* ]]; then
  echo "Path traversal detected" >&2
  exit 2
fi

# 機密ファイルチェック（推奨形式: hookSpecificOutput）
if echo "$FILE_PATH" | grep -qE '\.(env|key|pem)$|id_rsa|\.git/'; then
  echo "{\"hookSpecificOutput\":{\"hookEventName\":\"PreToolUse\",\"permissionDecision\":\"deny\",\"permissionDecisionReason\":\"機密ファイル $FILE_PATH への書き込みは禁止されています\"}}"
  exit 0
fi

exit 0
```

設定:
```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "\"$CLAUDE_PROJECT_DIR\"/.claude/hooks/protect-sensitive.sh"
          }
        ]
      }
    ]
  }
}
```

### Stop hook（品質ゲート）

```json
{
  "hooks": {
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "\"$CLAUDE_PROJECT_DIR\"/.claude/hooks/verify-complete.sh"
          }
        ]
      }
    ]
  }
}
```

`.claude/hooks/verify-complete.sh`:

```bash
#!/bin/bash
INPUT=$(cat)

# 必須: 無限ループ防止
if [ "$(echo "$INPUT" | jq -r '.stop_hook_active')" = "true" ]; then
  exit 0
fi

npm test 2>/dev/null && exit 0

echo "テスト失敗。修正してください" >&2
exit 2
```

### async hook（重い処理の非同期化）

テスト実行など重い処理を Claude をブロックせずに実行:

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "\"$CLAUDE_PROJECT_DIR\"/.claude/hooks/run-tests-async.sh",
            "async": true,
            "timeout": 300
          }
        ]
      }
    ]
  }
}
```

`.claude/hooks/run-tests-async.sh`:

```bash
#!/bin/bash
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

[[ "$FILE_PATH" != *.ts && "$FILE_PATH" != *.js ]] && exit 0

RESULT=$(npm test 2>&1)
if [ $? -eq 0 ]; then
  echo "{\"systemMessage\": \"テストパス: $FILE_PATH 編集後\"}"
else
  echo "{\"systemMessage\": \"テスト失敗: $FILE_PATH 編集後\\n$RESULT\"}"
fi
```

### tool input modification（入力の自動変換）

PreToolUse でツール入力を変換してから実行させる（v2.0.10+）:

```bash
#!/bin/bash
# grep → rg の自動変換
INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name')
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

if [[ "$TOOL" == "Bash" && "$COMMAND" == grep* ]]; then
  NEW_CMD="${COMMAND/grep /rg }"
  echo "{\"hookSpecificOutput\":{\"hookEventName\":\"PreToolUse\",\"permissionDecision\":\"allow\",\"permissionDecisionReason\":\"grep を rg に自動変換\",\"updatedInput\":{\"command\":\"$NEW_CMD\"}}}"
  exit 0
fi
exit 0
```

### SubagentStart hookでのコンテキスト注入

```json
{
  "hooks": {
    "SubagentStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "echo '{\"hookSpecificOutput\":{\"hookEventName\":\"SubagentStart\",\"additionalContext\":\"セキュリティガイドラインに従って操作してください\"}}'"
          }
        ]
      }
    ]
  }
}
```

### TeammateIdle hook（マルチエージェント品質ゲート）

Agent Team のチームメートがアイドル化する前にテストを確認:

```bash
#!/bin/bash
if ! npm test 2>&1; then
  echo "テストが失敗しています。修正してからidle化してください。" >&2
  exit 2  # idle化をブロック、チームメンバーは作業継続
fi
exit 0
```

## ベストプラクティス

| カテゴリ | 推奨事項 |
|---------|---------|
| パス | `"$CLAUDE_PROJECT_DIR"`で絶対パス参照 |
| 変数 | シェル変数は必ずダブルクォート |
| 性能 | PreToolUseは軽量処理のみ → PostToolUse → テスト等は`async: true` |
| 並列実行 | 同一イベントにマッチする複数hooksは並列実行される |
| スクリプト | 複雑なロジックは外部スクリプト化（`$CLAUDE_PROJECT_DIR/.claude/hooks/`配下） |
| セキュリティ | 入力検証、パストラバーサル（`..`）チェック、`subprocess`はリスト形式で |
| ブロック形式 | PreToolUseのブロックは`hookSpecificOutput`形式が推奨（`exit 2`も動作） |
| デバッグ | `/hooks`コマンドで登録確認、`claude --debug`で実行詳細確認 |
| 一括無効化 | 設定ファイルに`"disableAllHooks": true`で全hook無効化 |

## アンチパターン

| パターン | 問題点 | 対策 |
|---------|--------|------|
| PreToolUseで重い処理 | 全ツール呼び出しに遅延 | PostToolUse or `async: true`に移動 |
| 入力未検証 | tool_inputを信頼してはいけない | 適切なバリデーション・パストラバーサルチェック |
| Stopフックでの無限ループ | Claudeが再度Stopをトリガー | `stop_hook_active`チェック |
| 相対パス使用 | ディレクトリ移動で破損 | `$CLAUDE_PROJECT_DIR`使用 |
| exit 2 + JSON の混在 | exit 2時はstdout JSONは無視される | ブロック時は`exit 2 + stderr`か`exit 0 + hookSpecificOutput JSON` |
| `decision`/`reason` 旧形式 | 廃止予定 | `hookSpecificOutput.permissionDecision`形式に移行 |

## 段階的導入

1. **単一フックから開始**: 最も効果の高いPostToolUse formatterから
2. **手動テスト**: スクリプトを単体で動作確認
3. **ログ追加**: デバッグ用にログ出力を仕込む
4. **徐々に拡張**: 問題なければ追加フックを導入

## 参考資料

### 公式

- [Hooks reference (日本語)](https://code.claude.com/docs/ja/hooks) - 完全なAPIリファレンス
- [Hooks reference (English)](https://code.claude.com/docs/en/hooks) - 英語版（最新更新が早い）
- [Automate workflows with hooks](https://code.claude.com/docs/en/hooks-guide) - クイックスタートガイド

### コミュニティ

- [claude-code-hooks-mastery](https://github.com/disler/claude-code-hooks-mastery) - 13イベント完全実装・監査ログ
- [claude-code-hooks-multi-agent-observability](https://github.com/disler/claude-code-hooks-multi-agent-observability) - マルチエージェント監視
- [claude-code-hooks (karanb192)](https://github.com/karanb192/claude-code-hooks) - セキュリティhooks（262テスト）
- [Claude Code Hooks Complete Guide](https://aiorg.dev/blog/claude-code-hooks) - 20+の実装例
- [Claude Code Hooks for uv Projects](https://pydevtools.com/blog/claude-code-hooks-for-uv/) - PythonスクリプトによるHook実装
- [awesome-claude-code](https://github.com/hesreallyhim/awesome-claude-code) - コミュニティリソース集
