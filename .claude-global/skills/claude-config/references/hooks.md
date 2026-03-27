# Hooks リファレンス

## TOC
1. [イベント完全一覧（17種）](#イベント完全一覧)
2. [hookSpecificOutput 仕様](#hookspecificoutput-仕様)
3. [stdin JSON 構造](#stdin-json-構造)
4. [実装パターン](#実装パターン)
5. [環境変数](#環境変数)
6. [デバッグ・段階的導入](#デバッグ段階的導入)
7. [4つの Hook パターン](#4つの-hook-パターン)
8. [フィードバック速度階層](#フィードバック速度階層)
9. [アンチパターン](#アンチパターン)

---

## イベント完全一覧

| イベント | ブロック可否 | stdin に含まれる主要フィールド |
|---------|------------|------------------------------|
| `SessionStart` | 不可 | `session_id`, `cwd` |
| `UserPromptSubmit` | 可 | `prompt` |
| `PreToolUse` | 可 | `tool_name`, `tool_input` |
| `PermissionRequest` | 可 | `tool_name`, `tool_input`, `permission_type` |
| `PostToolUse` | 不可（フィードバックのみ） | `tool_name`, `tool_input`, `tool_response` |
| `PostToolUseFailure` | 不可（フィードバックのみ） | `tool_name`, `tool_input`, `error` |
| `Stop` | 可 | `stop_hook_active`, `last_assistant_message` |
| `SubagentStart` | 不可 | `session_id`, `parent_session_id` |
| `SubagentStop` | 可 | `session_id`, `last_assistant_message` |
| `TeammateIdle` | 可 | `teammate_name`, `session_id` |
| `TaskCompleted` | 可 | `task_id`, `task_subject` |
| `PreCompact` | 不可 | `context_size`, `token_count` |
| `SessionEnd` | 不可 | `session_id`, `duration_ms` |
| `Notification` | 不可 | `message`, `notification_type` |
| `ConfigChange` | 可（policy_settings除く） | `file_path`, `old_value`, `new_value` |
| `WorktreeCreate` | 可 | `worktree_path`, `branch` |
| `WorktreeRemove` | 可 | `worktree_path` |

ブロック方法: `exit 2 + stderr` → Claudeへフィードバック（古い方式）、`exit 0 + hookSpecificOutput JSON` → 構造化応答（推奨）

---

## hookSpecificOutput 仕様

exit 0 の stdout に出力する JSON。イベントごとに使えるフィールドが異なる。

### PreToolUse

```json
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "理由",
    "updatedInput": { "command": "変換後のコマンド" }
  }
}
```

| フィールド | 値 | 説明 |
|-----------|-----|------|
| `permissionDecision` | `"allow"` / `"deny"` | ツール実行の許可/拒否 |
| `permissionDecisionReason` | string | Claudeへの説明 |
| `updatedInput` | object | ツール入力の書き換え（v2.0.10+）。grep → rg 自動変換、パス正規化等 |

### SubagentStart / PostToolUse / SessionStart

```json
{ "hookSpecificOutput": { "hookEventName": "SubagentStart", "additionalContext": "コンテキスト文字列" } }
```

### Stop / SubagentStop

exit 2 + stderr が標準。hookSpecificOutput は使わない。

---

## stdin JSON 構造

すべてのhookはstdinでJSONを受け取る:

```bash
INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name // empty')
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
STOP_ACTIVE=$(echo "$INPUT" | jq -r '.stop_hook_active // false')
```

注意: `$()` コマンド置換の禁止は **Claude CodeのBash tool内でのみ適用**（サブシェルごとにパーミッション確認が発生するため）。hookスクリプト内のシェルでは `$()` は問題なく使用可能。

---

## 実装パターン

### stop_hook_active 無限ループ防止（必須）

Stop フックで exit 2 するとClaudeが再実行 → 再度Stopが発火するループ発生:

```bash
INPUT=$(cat)
if [ "$(echo "$INPUT" | jq -r '.stop_hook_active')" = "true" ]; then
  exit 0
fi
npm test || { echo "テスト失敗" >&2; exit 2; }
```

### async: true（ブロッキング回避）

```json
{ "type": "command", "command": "~/.dotfiles/.claude-global/hooks/run-tests-async.sh", "async": true, "timeout": 300 }
```

async hook の結果返却: stdout に `{"systemMessage": "表示メッセージ"}` を出力。

### tool input modification（updatedInput）

```bash
NEW_CMD="${COMMAND/grep /rg }"
printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow","permissionDecisionReason":"grep→rg自動変換","updatedInput":{"command":"%s"}}}' "$NEW_CMD"
exit 0
```

### prompt hook のレスポンス形式

LLM（Haiku）が stdout に返すべき JSON: `{ "ok": true }` / `{ "ok": false, "reason": "理由" }`
timeout 30秒。自然言語ルール判定向け。

### 機密ファイルブロック（二層防御）

```bash
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
if [[ "$FILE_PATH" == *".."* ]]; then
  echo "Path traversal detected" >&2; exit 2
fi
if echo "$FILE_PATH" | grep -qE '\.(env|key|pem)$|id_rsa'; then
  printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"機密ファイルへの書き込み禁止: %s"}}' "$FILE_PATH"
  exit 0
fi
```

### SubagentStart でのコンテキスト注入

```bash
printf '{"hookSpecificOutput":{"hookEventName":"SubagentStart","additionalContext":"セキュリティガイドライン: rm禁止、rmtrash使用"}}'
exit 0
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

スクリプト内パス参照は必ず `"$CLAUDE_PROJECT_DIR"/...` 形式（相対パス禁止）。

---

## デバッグ・段階的導入

```bash
/hooks          # 登録済みhookの確認
claude --debug  # 実行詳細の確認
```

一括無効化: `{ "disableAllHooks": true }`

**段階的導入手順**:
1. PostToolUse formatter から開始（最も安全、ブロックなし）
2. スクリプト単体テスト: `echo '{"tool_name":"Write","tool_input":{"file_path":"test.ts"}}' | ./hook.sh`
3. ログ追加: `echo "[hook] FILE=$FILE_PATH" >> /tmp/hook.log`
4. PreToolUse ガードは最後に追加（全ツール実行に影響）

**Exit code 規則**:

| コード | 意味 | 備考 |
|-------|------|------|
| `0` | 成功 | stdout が JSON なら解析される |
| `2` | ブロッキングエラー | stderr が Claude へフィードバック |
| その他 | 非ブロッキングエラー | 実行継続 |

`exit 2` 時は stdout の JSON は無視される。両方使う場合は `exit 0 + hookSpecificOutput deny` を使う。

---

## 4つの Hook パターン

| パターン | イベント | 目的 | 例 |
|---------|---------|------|-----|
| Safety Gates | PreToolUse | 危険操作をブロック | `rm -rf`防止、`.env`編集禁止、リンター設定保護 |
| Quality Loops | PostToolUse | 編集のたびにリント→自己修正 | 自動修正→残違反を additionalContext 注入 |
| Completion Gates | Stop | テスト通過まで完了させない | `stop_hook_active` で無限ループ防止必須 |
| Observability | 全イベント | 監視パイプラインにイベント流す | ログ集約、メトリクス収集 |

**Quality Loop の要点**: 自動修正を先に実行し、残った違反だけを `additionalContext` でフィードバック。

**リンター設定保護**: エージェントがリントエラー時にコード修正でなく設定変更するのを防ぐ。PreToolUse で `.eslintrc`, `biome.json`, `pyproject.toml` 等への書き込みを exit 2 でブロック。

---

## フィードバック速度階層

チェックを可能な限り速いレイヤーに移動させる:

| レイヤー | 速度 | 手段 |
|---------|------|------|
| PostToolUse Hook | ミリ秒 | 自動フォーマット + リント |
| プリコミットフック | 秒 | Lefthook 等でコミット前検出 |
| CI/CD | 分 | マージ前検出 |
| 人間レビュー | 時間〜日 | マージ後検出 |

---

## アンチパターン

| パターン | 問題 | 対策 |
|---------|------|------|
| Stop フックで `stop_hook_active` チェックなし | 無限ループ | 先頭で必ず確認 |
| exit 2 + stdout JSON の混在 | stdout は無視される | exit 2 → stderr、exit 0 → hookSpecificOutput |
| `decision`/`reason` 旧形式 | 廃止予定 | `hookSpecificOutput.permissionDecision` に移行 |
| PreToolUse で重い処理 | 全ツール呼び出しに遅延 | PostToolUse または `async: true` |
| 相対パス使用 | ディレクトリ移動で壊れる | `$CLAUDE_PROJECT_DIR` を使う |
| シェル変数をクォートなしで展開 | スペース含むパスで破損 | `"$FILE_PATH"` と必ずクォート |
| CLAUDE.md に「リンター実行せよ」と書くだけ | 「ほぼ毎回」止まり | Hook で「例外なく毎回」に強制 |
