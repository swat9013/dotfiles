---
name: setup-linter-hooks
description: プロジェクトに適したlinter/formatter hookを設定する。「lint設定」「linter hook」「formatter hook」「フォーマッタ設定」「コード整形hook」「hooks設定」「自動フォーマット」「pre-commit」「コードフォーマット」「保存時整形」「lint自動実行」と依頼された時に使用。プロジェクトの設定ファイルから自動検出してClaude Code hooks設定を生成。
---

# Setup Linter Hooks

プロジェクトの技術スタックを検出し、ファイル編集後に自動実行されるlinter/formatter hookを設定する。

## フロー概要

1. プロジェクト検出 → 技術スタック特定
2. linter/formatter特定 → 設定ファイルから抽出
3. hook設定生成 → 適切なパターン選択
4. 設定適用 → .claude/settings.json更新

---

## Phase 1: プロジェクト検出

以下のファイルを並列でチェック:

| ファイル | 言語/フレームワーク |
|---------|-------------------|
| `package.json` | JavaScript/TypeScript |
| `pyproject.toml`, `setup.py` | Python |
| `Cargo.toml` | Rust |
| `go.mod` | Go |
| `Gemfile` | Ruby |
| `composer.json` | PHP |

```bash
# 検出コマンド（参考）
ls -la package.json pyproject.toml Cargo.toml go.mod Gemfile composer.json 2>/dev/null
```

---

## Phase 2: linter/formatter検出

検出ファイル（`package.json`, `pyproject.toml`, `Cargo.toml`, `go.mod`等）の依存関係・設定セクションからlinter/formatterを特定する。

### 検出のポイント

- **JS/TS**: `package.json`の`devDependencies`キーで判別（`prettier`, `eslint`, `@biomejs/biome`, `dprint`）
- **Python**: `pyproject.toml`の`[tool.*]`セクション（`ruff`, `black`等）
- **Rust**: `cargo fmt`, `cargo clippy`は標準搭載（設定ファイル不要）
- **Go**: `gofmt`は標準搭載、`golangci-lint`は別途確認

### hook向けコマンド選定基準

- `--fix`/`--write`オプションがあるツールを優先（自動修正可能）
- fix不可のツール（flake8, mypy等）はhookに含めない
- 複数ツールがある場合: formatter → linterの順で実行

---

## Phase 3: hook設定生成

### パターン選択

| 条件 | パターン |
|-----|---------|
| 単一ツール | インラインコマンド |
| 複数ツール or 拡張子分岐 | 外部スクリプト |

### インラインパターン（単一ツール）

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "jq -r '.tool_input.file_path' | xargs -I {} sh -c '<COMMAND> \"{}\" 2>/dev/null || true'"
          }
        ]
      }
    ]
  }
}
```

### 外部スクリプトパターン（複数ツール）

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
  # 他の拡張子...
esac

exit 0
```

settings.json:

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

---

## Phase 4: 設定適用

### 確認事項

1. `.claude/settings.json`の存在確認
2. 既存hooks設定との競合チェック
3. ユーザー確認（AskUserQuestion）

### 適用手順

1. `.claude/`ディレクトリがなければ作成
2. 外部スクリプト方式なら`.claude/hooks/`作成
3. `settings.json`更新またはマージ
4. スクリプトに実行権限付与

```bash
chmod +x .claude/hooks/format-and-lint.sh
```

---

## 生成例

### Node.js + Prettier + ESLint

検出: `package.json` に `prettier`, `eslint` あり

生成:
- `.claude/hooks/format-and-lint.sh`
- `.claude/settings.json` hooks設定

### Python + Ruff

検出: `pyproject.toml` に `[tool.ruff]` あり

生成（インライン）:
```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "jq -r '.tool_input.file_path' | { read f; [[ \"$f\" == *.py ]] && ruff format \"$f\" && ruff check --fix \"$f\" 2>/dev/null || true; }"
          }
        ]
      }
    ]
  }
}
```

---

## 注意事項

- **既存設定の保持**: settings.jsonの他の設定を上書きしない
- **exit 0**: hookスクリプトは必ず0で終了（非0終了はClaudeにエラーとして報告される）
- **stderr抑制**: `2>/dev/null`でノイズを抑制
- **パス**: `$CLAUDE_PROJECT_DIR`で絶対パス参照

---

## 成功基準

1. `.claude/settings.json`にhooks設定が反映されている
2. 外部スクリプト方式の場合、スクリプトファイルが存在し実行権限がある
3. 対象拡張子のファイル編集後、linter/formatterが自動実行される

## 完了チェックリスト

- [ ] settings.jsonにhooks設定を追加した
- [ ] 外部スクリプトがあれば実行権限を付与した（chmod +x）
- [ ] 最小1ファイルで動作確認した（Write/Edit → hook実行を確認）
- [ ] 既存hooks設定との競合がないことを確認した

## 参考資料

- [Hooks設計ガイド](~/.dotfiles/docs/claude-code/hooks.md)
