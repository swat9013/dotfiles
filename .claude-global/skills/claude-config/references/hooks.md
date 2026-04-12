# Hooks リファレンス

## TOC
1. [イベント完全一覧（25種）](#イベント完全一覧)
2. [Hook 設定フィールド](#hook-設定フィールド)
3. [PreToolUse 権限決定の優先度](#pretooluse-権限決定の優先度)
4. [hookSpecificOutput 仕様](#hookspecificoutput-仕様)
5. [stdin JSON 構造](#stdin-json-構造)
6. [実装パターン](#実装パターン)
7. [環境変数](#環境変数)
8. [デバッグ・段階的導入](#デバッグ段階的導入)
設計パターン・アンチパターンは `hooks-patterns.md` 参照

---

## イベント完全一覧

| イベント | ブロック可否 | stdin に含まれる主要フィールド |
|---------|------------|------------------------------|
| `SessionStart` | 不可 | `session_id`, `cwd` |
| `UserPromptSubmit` | 可 | `prompt` |
| `PreToolUse` | 可 | `tool_name`, `tool_input` |
| `PermissionRequest` | 可 | `tool_name`, `tool_input`, `permission_type` |
| `PostToolUse` | 不可（FBのみ） | `tool_name`, `tool_input`, `tool_response` |
| `PostToolUseFailure` | 不可（FBのみ） | `tool_name`, `tool_input`, `error` |
| `Stop` | 可 | `stop_hook_active`, `last_assistant_message` |
| `StopFailure` | 不可 | `error` |
| `SubagentStart` | 不可 | `session_id`, `parent_session_id` |
| `SubagentStop` | 可 | `session_id`, `last_assistant_message` |
| `TeammateIdle` | 可 | `teammate_name`, `session_id` |
| `TaskCreated` | 可 | `task_id`, `task_subject`, `task_description`, `teammate_name` |
| `TaskCompleted` | 可 | `task_id`, `task_subject` |
| `PreCompact` | 不可 | `context_size`, `token_count` |
| `PostCompact` | 不可 | `context_size`, `token_count` |
| `SessionEnd` | 不可 | `session_id`, `duration_ms` |
| `Notification` | 不可 | `message`, `notification_type` |
| `ConfigChange` | 可（policy_settings除く） | `file_path`, `old_value`, `new_value` |
| `CwdChanged` | 不可 | `old_cwd`, `new_cwd` |
| `FileChanged` | 不可 | `file_path` |
| `InstructionsLoaded` | 不可 | `file_path` |
| `Elicitation` | 可 | MCP ツールの入力要求 |
| `ElicitationResult` | 可 | MCP 入力要求の結果 |
| `PermissionDenied` | 不可 | `tool_name`, `tool_input`, `permission_type` |
| `WorktreeCreate` | 可 | `worktree_path`, `branch` |
| `WorktreeRemove` | 可 | `worktree_path` |

ブロック方法: `exit 2 + stderr` → Claudeへフィードバック（古い方式）、`exit 0 + hookSpecificOutput JSON` → 構造化応答（推奨）

---

## Hook 設定フィールド

### `if` フィールド（v2.1.85+）

permission rule 構文で hook 実行を条件フィルタリング。プロセス起動オーバーヘッドを削減:

```json
{ "type": "command", "command": "~/.dotfiles/.claude-global/hooks/git-guard.sh", "if": "Bash(git *)" }
```

- `if` がマッチしない場合、hook スクリプトは起動されない（matcher は通常通り評価）
- 複合コマンド（`ls && git push`）、環境変数プレフィックス付き（`FOO=bar git push`）にも対応（v2.1.89修正）

### `once` / `async` フィールド

```json
{ "type": "command", "command": "...", "once": true }
{ "type": "command", "command": "...run-tests-async.sh", "async": true, "timeout": 300 }
```

`once`: セッション中1回だけ実行。初期化処理向け。`async`: バックグラウンド実行。結果返却は stdout に `{"systemMessage": "..."}` を出力。

### 出力サイズ制限（v2.1.89+）

hook 出力が **50K文字**を超えた場合、コンテキストに直接注入されずディスクに保存される。ファイルパス＋プレビューがコンテキストに渡される。

---

## PreToolUse 権限決定の優先度

複数 hook が異なる権限決定を返した場合: `deny > defer > ask > allow`

`defer`（v2.1.89+）: ヘッドレスセッションでツール呼び出しを一時停止し、`-p --resume` で再開・再評価可能。

---

## hookSpecificOutput 仕様

exit 0 の stdout に出力する JSON。イベントごとに使えるフィールドが異なる。

### PreToolUse

```json
{ "hookSpecificOutput": { "hookEventName": "PreToolUse", "permissionDecision": "deny", "permissionDecisionReason": "理由", "updatedInput": { "command": "変換後のコマンド" } } }
```

| フィールド | 値 | 説明 |
|-----------|-----|------|
| `permissionDecision` | `"allow"` / `"deny"` | ツール実行の許可/拒否 |
| `permissionDecisionReason` | string | Claudeへの説明 |
| `updatedInput` | object | ツール入力の書き換え（v2.0.10+）。grep → rg 自動変換、パス正規化等 |

**AskUserQuestion 自動応答**（v2.1.85+）: `tool_name: "AskUserQuestion"` 検出時に `updatedInput` + `permissionDecision: "allow"` を返すことでユーザー確認を自動処理可能。

### UserPromptSubmit

```json
{ "hookSpecificOutput": { "hookEventName": "UserPromptSubmit", "sessionTitle": "セッションタイトル" } }
```

`sessionTitle`（v2.1.94+）: セッションタイトルをプログラム的に設定。

### SubagentStart / PostToolUse / SessionStart

```json
{ "hookSpecificOutput": { "hookEventName": "SubagentStart", "additionalContext": "コンテキスト文字列" } }
```

Stop / SubagentStop は exit 2 + stderr が標準（hookSpecificOutput は使わない）。

---

## stdin JSON 構造

すべてのhookはstdinでJSONを受け取る。`INPUT=$(cat)` で受け取り、`printf '%s\n' "$INPUT" | jq -r '.tool_name // empty'` で各フィールドを取得。

注意: `$()` コマンド置換の禁止は **Claude CodeのBash tool内でのみ適用**。hookスクリプト内では `$()` は問題なく使用可能。

---

## 実装パターン

### stop_hook_active 無限ループ防止（必須）

Stop フックで exit 2 するとClaudeが再実行 → 再度Stopが発火するループ発生。先頭で `stop_hook_active` を確認して exit 0:

```bash
INPUT=$(cat)
[ "$(printf '%s\n' "$INPUT" | jq -r '.stop_hook_active')" = "true" ] && exit 0
npm test || { echo "テスト失敗" >&2; exit 2; }
```

### tool input modification / prompt hook

- `updatedInput`: `printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow","updatedInput":{"command":"%s"}}}' "${COMMAND/grep /rg }"`
- prompt hook: LLM（Haiku）が stdout に返すべき JSON: `{ "ok": true }` / `{ "ok": false, "reason": "理由" }`。timeout 30秒。

### 機密ファイルブロック / SubagentStart コンテキスト注入

```bash
# 機密ファイルブロック（PreToolUse）
FILE_PATH=$(printf '%s\n' "$INPUT" | jq -r '.tool_input.file_path // empty')
[[ "$FILE_PATH" == *".."* ]] && { echo "Path traversal" >&2; exit 2; }
printf '%s\n' "$FILE_PATH" | grep -qE '\.(env|key|pem)$|id_rsa' && \
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"機密ファイル禁止: %s"}}' "$FILE_PATH" && exit 0
# SubagentStart コンテキスト注入
printf '{"hookSpecificOutput":{"hookEventName":"SubagentStart","additionalContext":"機密ファイル書き込み禁止、破壊的git操作禁止"}}'; exit 0
```

同一イベントに複数の hooks マッチ → **並列実行**される。順序依存の処理は1つのスクリプトに集約。

---

## 環境変数

| 変数 | 説明 |
|------|------|
| `CLAUDE_PROJECT_DIR` | プロジェクトルートの絶対パス（hook内でのパス参照） |
| `CLAUDE_ENV_FILE` | SessionStart専用の環境変数永続化ファイル |
| `CLAUDE_CODE_REMOTE` | リモート環境なら `"true"` |
| `CLAUDE_PLUGIN_ROOT` | プラグインディレクトリパス |
| `CLAUDE_CODE_MCP_SERVER_NAME` | MCP サーバー名（`headersHelper` スクリプト用） |
| `CLAUDE_CODE_MCP_SERVER_URL` | MCP サーバーURL（`headersHelper` スクリプト用） |

スクリプト内パス参照は必ず `"$CLAUDE_PROJECT_DIR"/...` 形式（相対パス禁止）。

---

## デバッグ・段階的導入

`/hooks`（登録済みhook確認）、`claude --debug`（実行詳細確認）、一括無効化: `{ "disableAllHooks": true }`

**hookエラー診断（v2.1.98+）**: hookが非ゼロで終了した場合、stderrの最初の行がエラーメッセージに含まれるようになった。エラーの自己診断に活用可能。

**段階的導入手順**: PostToolUse formatter → スクリプト単体テスト(`echo '...' | ./hook.sh`) → ログ追加 → PreToolUse ガード（最後に追加）

**Exit code 規則**:

| コード | 意味 | 備考 |
|-------|------|------|
| `0` | 成功 | stdout が JSON なら解析 |
| `2` | ブロッキングエラー | stderr が Claude へフィードバック |
| その他 | 非ブロッキングエラー | 実行継続 |

### ハマりポイント

- **`set -e` 禁止**: jq パース失敗等で即座に non-zero exit → 「hook returned error」表示。`|| exit 0` で個別ガードする
- **`echo "$INPUT"` 危険**: PostToolUse の content はファイル全文を含む巨大 JSON になり得る。バックスラッシュ解釈・バッファ制限の問題があるため `printf '%s\n' "$INPUT"` を使う
- **実行環境差異**: 手動テストで再現しないエラーが発生し得る（PATH・環境変数・cwd が異なる）。外部コマンドはフルパス指定が安全
- **テンプレート**: `#!/bin/sh` + `INPUT=$(cat)` + `TOOL=$(printf '%s\n' "$INPUT" | jq -r '.tool_name' 2>/dev/null) || exit 0` + `exit 0`

