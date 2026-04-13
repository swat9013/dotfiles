# データソース詳細リファレンス（サードパーティツール）

GitHub Copilot Chat / Cline / Roo Code / Windsurf / Google Antigravity / OpenAI Codex / OpenCode のログ詳細。

## TOC
- [2. GitHub Copilot Chat](#2-github-copilot-chat)
- [3. Cline](#3-cline)
- [4. Roo Code](#4-roo-code)
- [5. Windsurf (Cascade)](#5-windsurf-cascade)
- [6. Google Antigravity](#6-google-antigravity)
- [7. OpenAI Codex（CLI）](#7-openai-codexcli)
- [8. OpenCode](#8-opencode)

---

## 2. GitHub Copilot Chat

### 保存場所
| OS | パス |
|----|------|
| Windows | `%APPDATA%\Code\User\workspaceStorage\*\state.vscdb` |
| macOS | `~/Library/Application Support/Code/User/workspaceStorage/*/state.vscdb` |
| Linux | `~/.config/Code/User/workspaceStorage/*/state.vscdb` |

### ファイル形式
SQLite データベース（`state.vscdb`）

### 抽出方法
```bash
sqlite3 "<path>/state.vscdb" "SELECT key FROM ItemTable WHERE key LIKE '%chat%';"
sqlite3 "<path>/state.vscdb" "SELECT value FROM ItemTable WHERE key = 'interactive.sessions';" 2>/dev/null
sqlite3 "<path>/state.vscdb" "SELECT value FROM ItemTable WHERE key = 'chat.ChatSessionStore.index';" 2>/dev/null
```

### 注意事項
- `sqlite3` コマンドが必要（Windows では Git Bash 付属のものや別途インストール）
- ワークスペースごとに別の `state.vscdb` が存在する
- セッションデータはJSON文字列としてvalueカラムに格納

---

## 3. Cline

### 保存場所
| OS | パス |
|----|------|
| Windows | `%APPDATA%\Code\User\globalStorage\saoudrizwan.claude-dev\` |
| macOS | `~/Library/Application Support/Code/User/globalStorage/saoudrizwan.claude-dev/` |
| Linux | `~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/` |

### ファイル構造
```
saoudrizwan.claude-dev/
├── state/taskHistory.json              # タスク履歴インデックス
└── tasks/{task-id}/
    ├── api_conversation_history.json   # API会話ログ
    ├── ui_messages.json                # UI表示メッセージ
    └── task_metadata.json              # タスクメタデータ
```

### 抽出方法
1. `taskHistory.json` を読み込んでタスク一覧を取得
2. 各タスクの `api_conversation_history.json` から `role: "human"` のメッセージを抽出
3. サンプリング: 最新20タスクに制限

---

## 4. Roo Code

### 保存場所
| OS | パス |
|----|------|
| Windows | `%APPDATA%\Code\User\globalStorage\RooVeterinaryInc.roo-cline\` |
| macOS | `~/Library/Application Support/Code/User/globalStorage/RooVeterinaryInc.roo-cline/` |
| Linux | `~/.config/Code/User/globalStorage/RooVeterinaryInc.roo-cline/` |

### ファイル構造
Cline と同一構造（Roo Code は Cline のフォーク）。

### 抽出方法
Cline と同じ手順。

---

## 5. Windsurf (Cascade)

### 保存場所
| OS | パス |
|----|------|
| Windows | `%USERPROFILE%\.codeium\windsurf\memories\` |
| macOS | `~/.codeium/windsurf/memories/` |
| Linux | `~/.codeium/windsurf/memories/` |

バックアップ（cascade-backup-utils 使用時）: `~/.cascade_backups/`

### ファイル形式
メモリファイル（テキスト形式）。会話の直接ログではなく、Cascadeが自動生成した要約・メモリ。

### 抽出方法
1. `~/.codeium/windsurf/memories/` 配下をGlobで検索
2. テキストファイルをReadで読み込み
3. `~/.cascade_backups/` が存在する場合はそちらも読み込み
4. メモリファイルのため、Cascadeの要約情報として扱う

### 注意事項
- Windsurf の会話ログ自体はローカルに直接保存されない場合がある
- memories/ はワークスペース単位で分離されている

---

## 6. Google Antigravity

### 保存場所
| OS | パス |
|----|------|
| Windows | `%USERPROFILE%\.gemini\antigravity\brain\` |
| macOS | `~/.gemini/antigravity/brain/` + `~/.gemini/antigravity/conversations/` |
| Linux | `~/.gemini/antigravity/brain/` |

### ファイル構造
```
~/.gemini/antigravity/
├── brain/{conversation-id}/.system_generated/logs/  # 会話ログ
└── conversations/*.pb                               # Protocol Buffers（バイナリ）
```

### 抽出方法
1. `~/.gemini/antigravity/brain/` 配下をGlobで探索
2. `.system_generated/logs/` 内のテキストファイルを読み込み
3. `.pb` ファイルはバイナリのため直接読み取り不可 → スキップ
4. テキスト形式のログファイルのみ対象

### 注意事項
- Antigravity は比較的新しいツールのためログ形式が変更される可能性がある
- Protocol Buffers 形式のファイルはスキップする

---

## 7. OpenAI Codex（CLI）

### 保存場所
| OS | パス |
|----|------|
| Windows | `%USERPROFILE%\.codex\sessions\` |
| macOS | `~/.codex/sessions/` |
| Linux | `~/.codex/sessions/` |

環境変数 `CODEX_HOME` が設定されている場合は、`$CODEX_HOME/sessions/` を使用する。

### ファイル構造
```
~/.codex/
├── state-v5.db              # スレッドメタデータ（SQLite）
└── sessions/YYYY/MM/DD/
    └── rollout-*.jsonl      # セッション別会話ログ
```

### rollout JSONL の形式
各行がJSONオブジェクト。主要な種別:
- **SessionMeta**: `{"SessionMeta": {"cwd": "/path/to/project", "model_provider": "openai", ...}}`
- **ResponseItem**: `{"ResponseItem": {"type": "message", "role": "user", "content": [{"type": "input_text", "text": "ユーザーの入力"}]}}`

### 抽出方法
1. `~/.codex/sessions/` 配下の `rollout-*.jsonl` を再帰的に走査（最新50件）
2. `SessionMeta` 行の `cwd` でプロジェクト情報を取得
3. `ResponseItem` で `type: "message"`, `role: "user"` のメッセージを抽出
4. `content` 配列から `type: "input_text"` または `type: "text"` のパートを連結（1ファイルあたり100件上限）

### 注意事項
- セッションはグローバル保存（プロジェクト別ディレクトリではない）
- プロジェクト情報は `SessionMeta` の `cwd` フィールドから取得

---

## 8. OpenCode

### 保存場所
| OS | パス |
|----|------|
| Windows | `%USERPROFILE%\.local\share\opencode\` |
| macOS | `~/.local/share/opencode/` |
| Linux | `~/.local/share/opencode/` |

環境変数 `XDG_DATA_HOME` が設定されている場合は `$XDG_DATA_HOME/opencode/` を使用する。SQLite データベース（`opencode.db` または `opencode-<channel>.db`）。

### 主要テーブル
- `project` - worktree, name 等のプロジェクト情報
- `session` - セッション単位のメタデータ
- `message` - role/data 等（data カラムはJSON）
- `part` - メッセージの各パート（data カラムはJSON）

### 抽出方法
1. `opencode.db` を優先、なければ `opencode-*.db` を探索
2. `session.parent_id IS NULL` の親セッションのみ（サブエージェントの子セッションは除外）
3. `message.data.role == "user"` のメッセージのみ対象
4. `part.data.type == "text"` かつ `synthetic != true` かつ `ignored != true` のパートを連結してプロンプト化
5. タイムスタンプは `message.time_created`（Unix epoch ミリ秒）を使用
