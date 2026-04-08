# データソース詳細リファレンス

各AIツールのログ保存場所・フォーマット・抽出方法の詳細。

## Contents
- [1. Claude Code（主要ソース）](#1-claude-code主要ソース)
- [2. GitHub Copilot Chat](#2-github-copilot-chat)
- [3. Cline](#3-cline)
- [4. Roo Code](#4-roo-code)
- [5. Windsurf (Cascade)](#5-windsurf-cascade)
- [6. Google Antigravity](#6-google-antigravity)
- [7. OpenAI Codex（CLI）](#7-openai-codexcli)
- [8. OpenCode](#8-opencode)
- [共通のサンプリング戦略](#共通のサンプリング戦略)

---

## 1. Claude Code（主要ソース）

### 保存場所
| OS | パス |
|----|------|
| Windows | `C:\Users\<user>\.claude\` |
| macOS | `~/.claude/` |
| Linux | `~/.claude/` |

### ファイル構造
```
~/.claude/
├── history.jsonl                          # 全プロジェクトの軽量インデックス
└── projects/
    └── {encoded-path}/                    # パス区切り(\, /, :) → - に変換
        ├── {session-uuid}.jsonl           # セッション別の完全な会話ログ
        └── {session-uuid}/
            └── subagents/                 # サブエージェントのログ
```

### history.jsonl の形式
```json
{"display":"ユーザーの入力テキスト","pastedContents":{},"timestamp":1759115795246,"project":"D:\\path\\to\\project"}
```

### プロジェクト別JSONL の形式
各行がJSON。ユーザーメッセージは `"type":"user"` を含む。
```json
{"type":"user","message":{"role":"user","content":[{"type":"text","text":"..."}]},"timestamp":"..."}
```

### 抽出方法（2ソース方式）

**ソース1: history.jsonl（CLI使用時）**
1. `history.jsonl` を読み込み、`display` フィールドからプロンプトを取得
2. `/clear` やファイルパスのみの行を除外
3. 収集済みの `sessionId` を記録（ソース2との重複排除用）

**ソース2: プロジェクト別セッションJSONL（VS Code拡張機能使用時）**
1. `projects/` 配下の各プロジェクトディレクトリを走査
2. 各ディレクトリの `*.jsonl` ファイルから `"type":"user"` のメッセージを抽出
3. `isMeta: true` のシステムメッセージはスキップ
4. `<ide_opened_file>`, `<local-command-stdout>` 等のシステムタグを除外
5. history.jsonl で収集済みのセッションIDはスキップ（重複排除）
6. 各プロジェクト最新5ファイル、1ファイルあたりユーザーメッセージ100件上限

**重要**: VS Code拡張機能で使用した場合、`history.jsonl` にはエントリが記録されず、
プロジェクト別セッションJSONLにのみ会話ログが保存される。両方のソースを読む必要がある。

### パスエンコード規則
プロジェクトディレクトリ名は、元のパスの `\`, `/`, `:` を `-` に変換。
例: `C:\Users\shinta\Documents\GitHub\yonshogen` → `c--Users-shinta-Documents-GitHub-yonshogen`

---

## 2〜8. サードパーティツール

GitHub Copilot Chat / Cline / Roo Code / Windsurf / Google Antigravity / OpenAI Codex / OpenCode の詳細は `data-sources-third-party.md` を参照。

---

## 共通のサンプリング戦略

大量のログデータを効率的に処理するため、以下の戦略を適用する:

| パラメータ | 値 | 理由 |
|-----------|-----|------|
| プロジェクトあたり最大ファイル数 | 5 | コンテキストウィンドウの制約 |
| ファイルあたり最大ユーザーメッセージ数 | 100 | 十分な分析量を確保しつつ制限 |
| Cline/Roo Code 最大タスク数 | 20 | 直近の活動に焦点 |
| 日数フィルタ | 引数で指定 | timestamp を現在時刻と比較 |

### 日数フィルタの適用方法
- Claude Code: `timestamp` フィールド（Unix epoch ミリ秒）で比較
- Cline/Roo Code: `task_metadata.json` のタイムスタンプで比較
- GitHub Copilot Chat: セッションデータ内のタイムスタンプで比較
- OpenAI Codex: `timestamp` フィールド（ISO 8601）で比較
- Windsurf/Antigravity: ファイルの更新日時で比較（正確なタイムスタンプが取れない場合）

### 存在チェックの順序
1. まず各ツールのベースディレクトリが存在するかをGlobまたはlsで確認
2. 存在するツールのみデータ収集を実行
3. 検出されなかったツールはレポートの「データソースサマリー」に「未検出」と記載
