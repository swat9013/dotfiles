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

| イベント | 発火タイミング | 主な用途 |
|---------|---------------|---------|
| `SessionStart` | セッション開始・再開時 | コンテキスト注入、環境設定 |
| `UserPromptSubmit` | プロンプト送信時 | 入力検証、追加コンテキスト |
| `PreToolUse` | ツール実行前 | コマンドブロック、入力検証 |
| `PermissionRequest` | 許可ダイアログ表示時 | 自動承認/拒否 |
| `PostToolUse` | ツール成功後 | **linter/formatter実行** |
| `PostToolUseFailure` | ツール失敗後 | エラーハンドリング |
| `Stop` | Claude応答完了時 | 品質ゲート、タスク検証 |
| `SubagentStop` | サブエージェント完了時 | 出力品質検証 |
| `PreCompact` | コンパクション前 | 状態保存 |
| `SessionEnd` | セッション終了時 | クリーンアップ、ログ |
| `Notification` | 通知送信時 | カスタム通知 |
| `Setup` | `--init`/`--maintenance`時 | 依存関係インストール |

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

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "jq -r '.tool_input.file_path' | grep -qE '\\.(env|key|pem)$' && echo 'Sensitive file blocked' >&2 && exit 2 || exit 0"
          }
        ]
      }
    ]
  }
}
```

## ベストプラクティス

| カテゴリ | 推奨事項 |
|---------|---------|
| パス | `"$CLAUDE_PROJECT_DIR"`で絶対パス参照 |
| 変数 | シェル変数は必ずダブルクォート |
| 性能 | PreToolUseは軽量処理のみ、重い処理はPostToolUseで |
| スクリプト | 複雑なロジックは外部スクリプト化 |
| セキュリティ | 入力検証、パストラバーサル（`..`）チェック |
| デバッグ | `claude --debug`で実行詳細確認 |

## アンチパターン

| パターン | 問題点 | 対策 |
|---------|--------|------|
| PreToolUseで重い処理 | 全ツール呼び出しに遅延 | PostToolUseに移動 |
| 入力未検証 | tool_inputを信頼してはいけない | 適切なバリデーション |
| Stopフックでの無限ループ | Claudeが再度Stopをトリガー | `stop_hook_active`チェック |
| 相対パス使用 | ディレクトリ移動で破損 | `$CLAUDE_PROJECT_DIR`使用 |

## 段階的導入

1. **単一フックから開始**: 最も効果の高いPostToolUse formatterから
2. **手動テスト**: スクリプトを単体で動作確認
3. **ログ追加**: デバッグ用にログ出力を仕込む
4. **徐々に拡張**: 問題なければ追加フックを導入

## 参考資料

### 公式

- [Hooks reference](https://code.claude.com/docs/en/hooks) - 完全なAPIリファレンス
- [Get started with hooks](https://code.claude.com/docs/en/hooks-guide) - クイックスタート

### コミュニティ

- [everything-claude-code](https://github.com/affaan-m/everything-claude-code) - 包括的な設定例
- [claude-code-hooks-mastery](https://github.com/disler/claude-code-hooks-mastery) - フック活用パターン
