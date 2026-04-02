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
9. [4つの Hook パターン](#4つの-hook-パターン)
10. [フィードバック速度階層](#フィードバック速度階層)
11. [アンチパターン](#アンチパターン)

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
| `WorktreeCreate` | 可 | `worktree_path`, `branch` |
| `WorktreeRemove` | 可 | `worktree_path` |

ブロック方法: `exit 2 + stderr` → Claudeへフィードバック（古い方式）、`exit 0 + hookSpecificOutput JSON` → 構造化応答（推奨）

---

## Hook 設定フィールド

### `if` フィールド（v2.1.85+）

permission rule 構文で hook 実行を条件フィルタリング。プロセス起動オーバーヘッドを削減:

```json
{
  "hooks": {
    "PreToolUse": [{
      "matcher": "Bash",
      "hooks": [{
        "type": "command",
        "command": "~/.dotfiles/.claude-global/hooks/git-guard.sh",
        "if": "Bash(git *)"
      }]
    }]
  }
}
```

- `if` がマッチしない場合、hook スクリプトは起動されない（matcher は通常通り評価）
- 複合コマンド（`ls && git push`）、環境変数プレフィックス付き（`FOO=bar git push`）にも対応（v2.1.89修正）

### `once` フィールド

```json
{ "type": "command", "command": "...", "once": true }
```

セッション中1回だけ実行。初期化処理やスキルの1回限り設定向け。

### `async` フィールド

```json
{ "type": "command", "command": "~/.dotfiles/.claude-global/hooks/run-tests-async.sh", "async": true, "timeout": 300 }
```

バックグラウンド実行。結果返却: stdout に `{"systemMessage": "表示メッセージ"}` を出力。

### 出力サイズ制限（v2.1.89+）

hook 出力が **50K文字**を超えた場合、コンテキストに直接注入されずディスクに保存される。ファイルパス＋プレビューがコンテキストに渡される。

---

## PreToolUse 権限決定の優先度

複数の hook が異なる権限決定を返した場合の優先度:

```
deny > defer > ask > allow
```

- `defer`（v2.1.89+）: ヘッドレスセッションでツール呼び出しを一時停止し、`-p --resume` で再開・再評価可能

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

**AskUserQuestion 自動応答**（v2.1.85+）: PreToolUse で `tool_name: "AskUserQuestion"` を検出し、`updatedInput` + `permissionDecision: "allow"` を返すことで、ユーザー確認を自動処理可能。

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
printf '{"hookSpecificOutput":{"hookEventName":"SubagentStart","additionalContext":"セキュリティガイドライン: 機密ファイル書き込み禁止、破壊的git操作禁止"}}'
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
| `CLAUDE_CODE_MCP_SERVER_NAME` | MCP サーバー名（`headersHelper` スクリプト用） |
| `CLAUDE_CODE_MCP_SERVER_URL` | MCP サーバーURL（`headersHelper` スクリプト用） |

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

**デバッグ用入力キャプチャ**（一時的に追加→原因特定後に削除）:

```bash
INPUT=$(cat)
printf '%s\n' "$INPUT" > /tmp/hook-debug.json  # 実際の入力を保存
```

**Exit code 規則**:

| コード | 意味 | 備考 |
|-------|------|------|
| `0` | 成功 | stdout が JSON なら解析される |
| `2` | ブロッキングエラー | stderr が Claude へフィードバック |
| その他 | 非ブロッキングエラー | 「hook returned error」表示、実行は継続 |

`exit 2` 時は stdout の JSON は無視される。両方使う場合は `exit 0 + hookSpecificOutput deny` を使う。

### ハマりポイント

**`set -e` はhookスクリプトで使わない**:
hookは「失敗しても本体処理を妨げない」べき。`set -e` があると jq パース失敗等で即座に non-zero exit → 「hook returned error」表示。代わりに `|| exit 0` で個別ガードする。

**`echo "$INPUT"` は任意JSONに対して安全でない**:
PostToolUse の Write 入力は `tool_input.content` にファイル全文を含むため、JSONが巨大になる。`echo` はバックスラッシュ解釈やバッファ制限の問題があるため、`printf '%s\n' "$INPUT"` を使う。

**hookの実行環境はユーザーシェルと異なる**:
手動テスト（`echo '...' | ./hook.sh`）では再現しないエラーがhook実行時に発生し得る。PATH、環境変数、カレントディレクトリが異なる可能性がある。外部コマンドはフルパス指定が安全。

**安全なhookスクリプトのテンプレート**:

```bash
#!/bin/sh
# set -e は使わない

INPUT=$(cat)
TOOL=$(printf '%s\n' "$INPUT" | jq -r '.tool_name' 2>/dev/null) || exit 0
# ... 処理 ...
exit 0
```

---

## 4つの Hook パターン

| パターン | イベント | 目的 | 例 |
|---------|---------|------|-----|
| Safety Gates | PreToolUse | 危険操作をブロック | `rm -rf`防止、`.env`編集禁止、リンター設定保護 |
| Quality Loops | PostToolUse | 編集のたびにリント→自己修正 | 自動修正→残違反を additionalContext 注入 |
| Completion Gates | Stop | テスト通過まで完了させない | `stop_hook_active` で無限ループ防止必須 |
| Observability | 全イベント | 監視パイプラインにイベント流す | ログ集約、メトリクス収集 |

**Quality Loop の要点**: 自動修正を先に実行し、残った違反だけを `additionalContext` でフィードバック。

**エラーメッセージ構造化**: hook出力を修正指示として設計する — WHY（違反理由+ADR/ルール参照）、FIX（具体的修正手順）、EXAMPLE（Bad→Good）。生のlinter出力より修正精度が向上する。

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

**エスカレーション原則**: 同一違反3回繰り返し → 次のエンフォースメントレベルに昇格（L1 ドキュメント指示 → L2 AI検証 → L3 ツール自動検証 → L4 構造テスト/CI）。

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
| `set -e` をhookスクリプトで使用 | jq失敗等で即死→「hook returned error」 | `|| exit 0` で個別ガード |
| `echo "$INPUT"` でJSON受け渡し | 巨大JSON・バックスラッシュで破損 | `printf '%s\n' "$INPUT"` を使う |
