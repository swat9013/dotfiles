# データソース詳細リファレンス（サードパーティツール）

GitHub Copilot Chat / Cline / Roo Code / Windsurf / Google Antigravity / OpenAI Codex / OpenCode のログ詳細。

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
# 利用可能なキーの一覧を取得
sqlite3 "<path>/state.vscdb" "SELECT key FROM ItemTable WHERE key LIKE '%chat%';"

# チャットセッションインデックスを取得
sqlite3 "<path>/state.vscdb" "SELECT value FROM ItemTable WHERE key = 'interactive.sessions';" 2>/dev/null

# 代替キー（バージョンにより異なる）
sqlite3 "<path>/state.vscdb" "SELECT value FROM ItemTable WHERE key = 'chat.ChatSessionStore.index';" 2>/dev/null
```

### 注意事項
- `sqlite3` コマンドが必要（Windows では Git Bash 付属のものや別途インストール）
- ワークスペースごとに別の `state.vscdb` が存在する
- セッションデータはJSON文字列としてvalueカラムに格納
- ユーザーのプロンプトは `request` や `message` フィールドに含まれる

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
├── state/
│   └── taskHistory.json              # タスク履歴インデックス
└── tasks/
    └── {task-id}/
        ├── api_conversation_history.json  # API会話ログ
        ├── ui_messages.json               # UI表示メッセージ
        └── task_metadata.json             # タスクメタデータ
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
4. メモリファイルのため、直接のプロンプトではなくCascadeの要約情報として扱う

### 注意事項
- Windsurf の会話ログ自体はローカルに直接保存されない場合がある
- memories/ はワークスペース単位で分離されている
- メモリの内容は要約であり、元のプロンプトとは異なる

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
├── brain/
│   └── {conversation-id}/
│       └── .system_generated/
│           └── logs/                  # 会話ログ
└── conversations/
    └── *.pb                           # Protocol Buffers 形式
```

### 抽出方法
1. `~/.gemini/antigravity/brain/` 配下をGlobで探索
2. `.system_generated/logs/` 内のテキストファイルを読み込み
3. `.pb` ファイル（Protocol Buffers）はバイナリのため直接読み取り不可 → スキップ
4. テキスト形式のログファイルのみ対象

### 注意事項
- Antigravity は比較的新しいツールのため、ログ形式が変更される可能性がある
- `.gemini/` フォルダが削除されると会話リストは残るが内容は読めなくなる（既知のバグ）
- Protocol Buffers 形式のファイルはテキストとして読めないためスキップする

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
├── config.toml                                    # 設定ファイル
├── state-v5.db                                    # スレッドメタデータ（SQLite）
├── session_index.jsonl                            # セッションインデックス
└── sessions/
    └── YYYY/MM/DD/
        └── rollout-YYYY-MM-DDThh-mm-ss-<id>.jsonl # セッション別会話ログ
```

### rollout JSONL の形式
各行がJSONオブジェクト。行の種別は以下のキーで判別する:

**SessionMeta（セッション開始時のメタ情報）**
```json
{"timestamp": "2025-06-15T10:30:00.123Z", "SessionMeta": {"cwd": "/path/to/project", "model_provider": "openai", ...}}
```

**ResponseItem（会話アイテム）**
```json
{"timestamp": "2025-06-15T10:30:01.000Z", "ResponseItem": {"type": "message", "role": "user", "content": [{"type": "input_text", "text": "ユーザーの入力"}]}}
```

### 抽出方法
1. `~/.codex/sessions/` 配下の `rollout-*.jsonl` を再帰的に走査（最新50件）
2. 各ファイルから `SessionMeta` 行の `cwd` でプロジェクト情報を取得
3. `ResponseItem` で `type: "message"`, `role: "user"` のメッセージを抽出
4. `content` 配列から `type: "input_text"` または `type: "text"` のパートを連結
5. 1ファイルあたりユーザーメッセージ100件上限

### 注意事項
- セッションはグローバル保存（プロジェクト別ディレクトリではない）
- プロジェクト情報は `SessionMeta` の `cwd` フィールドから取得
- `state-v5.db` にもスレッドメタデータがあるが、会話内容は rollout JSONL に保存される
- Codex は Rust 製のため、JSONL のキー名が CamelCase（`SessionMeta`, `ResponseItem`）

---

## 8. OpenCode

### 保存場所
| OS | パス |
|----|------|
| Windows | `%USERPROFILE%\.local\share\opencode\` |
| macOS | `~/.local/share/opencode/` |
| Linux | `~/.local/share/opencode/` |

環境変数 `XDG_DATA_HOME` が設定されている場合は、`$XDG_DATA_HOME/opencode/` を使用する。

### ファイル形式
SQLite データベース（`opencode.db` または `opencode-<channel>.db`）

### 主要テーブル
- `project` - `worktree`, `name` などのプロジェクト情報
- `session` - セッション単位のメタデータ
- `message` - メッセージ単位の情報。`data` カラムはJSON
- `part` - メッセージの各パート。`data` カラムはJSON

### 抽出方法
1. `opencode.db` を優先し、存在しない場合は `opencode-*.db` を探索
2. `project`, `session`, `message`, `part` を結合
3. `session.parent_id IS NULL` の親セッションのみ対象（サブエージェントの子セッションは除外）
4. `message.data.role == "user"` のメッセージのみ対象
5. `part.data.type == "text"` かつ `part.data.synthetic != true` かつ `part.data.ignored != true` のパートを連結してプロンプト化
6. タイムスタンプは `message.time_created`（Unix epoch ミリ秒）を使用
7. プロジェクト名は `project.worktree` の basename を使用
