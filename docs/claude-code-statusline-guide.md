# Claude Code Status Line カスタマイズガイド

## 概要

Claude Code の Status Line は、CLI 画面下部に表示されるカスタマイズ可能なステータス表示機能。シェルの PS1 プロンプトに似たコンセプトで、セッション情報をリアルタイムに表示する。

**主な用途:**
- 現在のモデル、作業ディレクトリの表示
- コンテキストウィンドウ使用率のモニタリング
- Git ブランチ・変更状況の確認
- セッションコスト・時間の追跡

## 設定方法

### 自動セットアップ（推奨）

```bash
/statusline
```

オプション指定も可能:
```bash
/statusline show the model name in orange
```

### 手動設定

`.claude/settings.json` に以下を追加:

```json
{
  "statusLine": {
    "type": "command",
    "command": "~/.dotfiles/.claude-global/statusline.sh",
    "padding": 0
  }
}
```

**設定ファイルの優先順位:**
1. プロジェクト固有: `.claude/settings.json`
2. ユーザー全体: `~/.claude/settings.json`

**padding オプション:**
- `0`: 端末幅いっぱいまで使用
- 未指定（デフォルト）: 左右にパディング

## JSON 入力データ仕様

Status Line スクリプトは stdin から JSON 形式でセッション情報を受け取る。

### 完全なデータ構造

```json
{
  "hook_event_name": "Status",
  "session_id": "abc123...",
  "transcript_path": "/path/to/transcript.json",
  "cwd": "/current/working/directory",
  "model": {
    "id": "claude-opus-4-5-20251101",
    "display_name": "Opus"
  },
  "workspace": {
    "current_dir": "/current/working/directory",
    "project_dir": "/original/project/directory"
  },
  "version": "1.0.80",
  "output_style": {
    "name": "default"
  },
  "cost": {
    "total_cost_usd": 0.01234,
    "total_duration_ms": 45000,
    "total_api_duration_ms": 2300,
    "total_lines_added": 156,
    "total_lines_removed": 23
  },
  "context_window": {
    "total_input_tokens": 15234,
    "total_output_tokens": 4521,
    "context_window_size": 200000,
    "current_usage": {
      "input_tokens": 8500,
      "output_tokens": 1200,
      "cache_creation_input_tokens": 5000,
      "cache_read_input_tokens": 2000
    }
  }
}
```

### 主要フィールド一覧

| フィールド | 説明 | 例 |
|-----------|------|-----|
| `.model.display_name` | 表示用モデル名 | "Opus", "Haiku" |
| `.model.id` | モデルID | "claude-opus-4-5-20251101" |
| `.workspace.current_dir` | 現在の作業ディレクトリ | "/Users/name/project" |
| `.workspace.project_dir` | プロジェクトディレクトリ | "/Users/name/project" |
| `.cwd` | 作業ディレクトリ（代替） | "/Users/name/project" |
| `.version` | Claude Code バージョン | "1.0.80" |
| `.session_id` | セッションID | "abc123..." |
| `.transcript_path` | トランスクリプトパス | "~/.claude/projects/.../session.json" |

### コスト関連

| フィールド | 説明 |
|-----------|------|
| `.cost.total_cost_usd` | セッション総コスト（USD） |
| `.cost.total_duration_ms` | 総実行時間（ミリ秒） |
| `.cost.total_api_duration_ms` | API呼び出し時間（ミリ秒） |
| `.cost.total_lines_added` | 追加コード行数 |
| `.cost.total_lines_removed` | 削除コード行数 |

### コンテキストウィンドウ関連

| フィールド | 説明 |
|-----------|------|
| `.context_window.context_window_size` | 最大コンテキストサイズ |
| `.context_window.total_input_tokens` | 累積入力トークン |
| `.context_window.total_output_tokens` | 累積出力トークン |
| `.context_window.current_usage.input_tokens` | 現在の入力トークン |
| `.context_window.current_usage.output_tokens` | 現在の出力トークン |
| `.context_window.current_usage.cache_creation_input_tokens` | キャッシュ作成トークン |
| `.context_window.current_usage.cache_read_input_tokens` | キャッシュ読み込みトークン |

**重要:** コンテキスト使用率の計算には `total_input_tokens`（累積）ではなく `current_usage` を使用する。

## 実装例

### 基本実装（Bash）

```bash
#!/bin/bash
input=$(cat)

MODEL=$(echo "$input" | jq -r '.model.display_name')
DIR=$(echo "$input" | jq -r '.workspace.current_dir')

echo "[$MODEL] ${DIR##*/}"
```

### Git統合版

```bash
#!/bin/bash
set -euo pipefail

# ANSI colors
GREEN='\033[32m'
YELLOW='\033[33m'
RESET='\033[0m'

if ! command -v jq &> /dev/null; then
    echo "[Claude]"
    exit 0
fi

input=$(cat)

MODEL=$(echo "$input" | jq -r '.model.display_name // "Claude"')
CURRENT_DIR=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // "."')

DIR_NAME="${CURRENT_DIR##*/}"

GIT_INFO=""
if git -C "$CURRENT_DIR" rev-parse --git-dir > /dev/null 2>&1; then
    BRANCH=$(git -C "$CURRENT_DIR" --no-optional-locks branch --show-current 2>/dev/null)
    if [ -n "$BRANCH" ]; then
        DIRTY=""
        if ! git -C "$CURRENT_DIR" --no-optional-locks diff --quiet 2>/dev/null; then
            DIRTY="*"
        fi
        GIT_INFO=" ${YELLOW}git:${BRANCH}${DIRTY}${RESET}"
    fi
fi

printf "%b\n" "${GREEN}${DIR_NAME}${RESET}${GIT_INFO}"
```

### コンテキスト使用率付き

```bash
#!/bin/bash
input=$(cat)

MODEL=$(echo "$input" | jq -r '.model.display_name')
CONTEXT_SIZE=$(echo "$input" | jq -r '.context_window.context_window_size // 200000')
USAGE=$(echo "$input" | jq '.context_window.current_usage')

if [ "$USAGE" != "null" ]; then
    TOKENS=$(echo "$USAGE" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens')
    PERCENT=$((TOKENS * 100 / CONTEXT_SIZE))
else
    PERCENT=0
fi

# Color based on usage
if [ "$PERCENT" -lt 50 ]; then
    COLOR='\033[32m'  # Green
elif [ "$PERCENT" -lt 80 ]; then
    COLOR='\033[33m'  # Yellow
else
    COLOR='\033[31m'  # Red
fi

printf "[$MODEL] ${COLOR}Ctx:${PERCENT}%%\033[0m\n"
```

### Python実装

```python
#!/usr/bin/env python3
import json
import sys
import os
import subprocess

data = json.load(sys.stdin)

model = data['model']['display_name']
current_dir = data.get('workspace', {}).get('current_dir', os.getcwd())
dir_name = os.path.basename(current_dir)

# Git branch
git_branch = ""
try:
    result = subprocess.run(
        ['git', '-C', current_dir, 'branch', '--show-current'],
        capture_output=True, text=True
    )
    if result.returncode == 0 and result.stdout.strip():
        git_branch = f" | git:{result.stdout.strip()}"
except:
    pass

print(f"[{model}] {dir_name}{git_branch}")
```

## ANSI カラーコード

Status Line は ANSI エスケープシーケンスをサポート:

| 色 | コード | 用途例 |
|----|--------|--------|
| 緑 | `\033[32m` | 正常状態、低使用率 |
| 黄 | `\033[33m` | 警告、中使用率 |
| 赤 | `\033[31m` | エラー、高使用率 |
| シアン | `\033[36m` | 情報 |
| リセット | `\033[0m` | 色をリセット |

## サードパーティツール

### ccstatusline

高機能な TUI ベースの設定ツール。

```bash
# npm
npx ccstatusline@latest

# Bun（推奨）
bunx ccstatusline@latest
```

**特徴:**
- Powerline スタイルのセパレータ
- マルチライン対応
- 24+ ウィジェット
- リアルタイムプレビュー
- React/Ink ベースの対話型 TUI

**公式:** https://github.com/sirmalloc/ccstatusline

### claude-code-statusline

TOML ベースの設定ファイルを使用。

**特徴:**
- `~/.claude/statusline/Config.toml` で一元管理
- テーマシステム
- 設定のライブリロード

**公式:** https://github.com/rz1989s/claude-code-statusline

## 制限事項と注意点

### パフォーマンス

- **更新頻度:** 最大 300ms ごと（レート制限あり）
- **実行時間:** 長時間実行のスクリプトは避ける
- 外部コマンド呼び出しは最小限に

### 出力制限

- **stdout のみ:** stderr は無視される
- **1行のみ:** 複数行出力は最初の1行のみ表示
- **端末幅:** 長すぎる出力は切り詰められる

### Git 操作

- `--no-optional-locks` でロック問題を回避
- `-C "$CURRENT_DIR"` で正しいディレクトリを指定

### 依存関係

- `jq`: JSON パース用（必須推奨）
- `git`: Git 情報取得用（オプション）

### jq 未インストール時のフォールバック

```bash
if ! command -v jq &> /dev/null; then
    echo "[Claude]"
    exit 0
fi
```

## デバッグ方法

### スクリプトテスト

```bash
# モック JSON でテスト
echo '{"model":{"display_name":"Test"},"workspace":{"current_dir":"/test"}}' | ./statusline.sh
```

### JSON 入力の確認

```bash
# スクリプト内で JSON をファイルに保存
echo "$input" > /tmp/claude-statusline-debug.json
```

### 権限確認

```bash
# 実行権限の確認
ls -la ~/.dotfiles/.claude-global/statusline.sh

# 権限付与
chmod +x ~/.dotfiles/.claude-global/statusline.sh
```

## このリポジトリの実装

`.claude-global/statusline.sh` の実装内容:

**表示形式:**
```
ディレクトリ git:ブランチ* | Ctx:XX% | "セッションタイトル..."
```

**カラースキーム:**
- ディレクトリ: 緑
- Git ブランチ: 黄（変更ありは `*` 付き）
- コンテキスト使用率: 緑 (0-50%), 黄 (50-80%), 赤 (80%+)
- セッションタイトル: シアン

**settings.json 設定:**
```json
{
  "statusLine": {
    "type": "command",
    "command": "~/.dotfiles/.claude-global/statusline.sh",
    "padding": 0
  }
}
```

## 参考資料

- [公式ドキュメント](https://code.claude.com/docs/en/statusline)
- [ccstatusline](https://github.com/sirmalloc/ccstatusline)
- [claude-code-statusline](https://github.com/rz1989s/claude-code-statusline)
- [カスタマイズ解説記事](https://www.lexo.ch/blog/2025/12/customize-your-claude-code-status-line/)
- [Status Line 活用記事](https://medium.com/@joe.njenga/how-im-using-claude-code-status-line-new-feature-to-keep-context-96a4adf21728)
