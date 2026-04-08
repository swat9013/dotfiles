# Agent Teams リファレンス

## TOC
1. [概要・有効化](#概要有効化)
2. [Subagents との使い分け](#subagents-との使い分け)
3. [アーキテクチャ](#アーキテクチャ)
4. [表示モード](#表示モード)
5. [Subagent 定義の再利用](#subagent-定義の再利用)
6. [Hooks](#hooks)
7. [ベストプラクティス](#ベストプラクティス)
8. [アンチパターン](#アンチパターン)
9. [既知の制限事項](#既知の制限事項)

---

## 概要・有効化

**ステータス**: Experimental（初出 v2.1.32、デフォルト無効）

```json
// settings.json
{
  "env": {
    "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS": "1"
  }
}
```

CLI: `claude --teammate-mode in-process`

---

## Subagents との使い分け

| 比較項目 | Subagents | Agent Teams |
|---------|-----------|-------------|
| コンテキスト | 独自ウィンドウ・結果を親に返す | 独自ウィンドウ・完全独立 |
| コミュニケーション | メインエージェントへの報告のみ | 相互直接メッセージ可能 |
| 調整 | メインエージェントが全作業管理 | 共有タスクリストで自己調整 |
| トークンコスト | 低（結果が要約される） | 高（各 teammate が独立インスタンス） |

**Subagents を選ぶ条件**: 結果だけが重要、独立した集中タスク、コスト重視
**Agent Teams を選ぶ条件**: 並列探索、知見の共有・議論が必要、PR レビュー・バグ調査・並列実装

---

## アーキテクチャ

| コンポーネント | 役割 |
|--------------|------|
| Team lead | チーム作成・teammate スポーン・調整を行うメインセッション |
| Teammates | 各自が独立コンテキストウィンドウを持つインスタンス |
| Task list | 全エージェントが参照・クレームできる共有リスト |
| Mailbox | エージェント間の直接メッセージングシステム |

**ストレージ**（実装詳細、Experimental のため変更可能性あり）: Team config → `~/.claude/teams/{team-name}/config.json` / Task list → `~/.claude/tasks/{team-name}/`

---

## 表示モード

| モード | 説明 | 前提条件 |
|--------|------|---------|
| `in-process` | 全 teammate がメインターミナル内。Shift+Down でサイクル | なし |
| `tmux` | 各 teammate が独立ペインを持つ | tmux または iTerm2 |
| `auto` | tmux セッション内なら split panes、そうでなければ in-process | - |

```json
// ~/.claude.json
{ "teammateMode": "in-process" }
```

> split-pane モードは VS Code 統合ターミナル・Windows Terminal・Ghostty では非対応

---

## Subagent 定義の再利用

teammate スポーン時に subagent タイプ名を指定すると:

- `tools` allowlist と `model` を継承
- body が teammate のシステムプロンプトに追記（置き換えではなく）
- `SendMessage` 等のチーム調整ツールは常に利用可能（`tools` 制限に関係なく）
- **注意**: subagent 定義の `skills` / `mcpServers` は teammate として再利用する場合は**無視される**（subagent 単体での使用時は有効）

### Plan approval ワークフロー

teammate に plan approval を要求すると:
- プランが承認されるまで plan mode（read-only）を維持
- lead が承認/却下（フィードバック付き）
- 承認後に実装開始

---

## Hooks

| イベント | タイミング | exit code 2 の効果 |
|---------|----------|-------------------|
| `TeammateIdle` | teammate がアイドル状態になる前 | フィードバックを送り作業継続 |
| `TaskCreated` | タスク作成時 | 作成をブロックしフィードバック送信 |
| `TaskCompleted` | タスク完了マーク時 | 完了をブロックしフィードバック送信 |

---

## ベストプラクティス

1. **実装前にプランを作る**: lead が全スコープを把握してから teammate をスポーン。計画なしではトークン浪費
2. **品質基準を最初に設定**: lead が各 teammate にトリクルダウン
3. **チームサイズは 3〜5 人**: トークンコストが線形スケール。5〜6 tasks/teammate が理想粒度
4. **タスクを適切なサイズに**: 小さすぎ→調整オーバーヘッド過大 / 大きすぎ→check-in なしのリスク増大
5. **spawn 時に十分なコンテキストを渡す**: teammate は lead の会話履歴を引き継がない
6. **ファイル競合を避ける**: 各 teammate が異なるファイルセットを担当。または `isolation: worktree` を使用
7. **Research & Review から始める**: コード記述を伴わないタスクが最適
8. **監視と方向修正を行う**: 長時間放置はリスク
9. **Plan Approval を高リスクタスクに使う**: teammate を read-only プランモードで開始し、lead が承認後に実装開始

---

## アンチパターン

| パターン | 問題 | 対策 |
|---------|------|------|
| Lead がタスク完了前に自ら実装を始める | 委譲の目的が失われる | `Wait for teammates to complete before proceeding` と指示 |
| 軽量・逐次タスクにチームを使う | 協調オーバーヘッドが上回る | 単一セッションまたは Subagents を使う |
| タスクを粗くしすぎる | 方向修正なしに長時間走る | 「明確な成果物を持つ自己完結単位」に分割 |

---

## 既知の制限事項

| 制限 | 詳細 |
|------|------|
| `/resume` と `/rewind` | in-process teammates を復元しない |
| タスクステータス | 遅延することがある（完了済みでも stuck 表示） |
| シャットダウン | 現在のリクエスト/ツール呼び出し完了まで待機 |
| 1セッション1チーム | 複数チームの同時運用不可 |
| Nested teams | teammate は自分のチームや teammate をスポーン不可 |
| Lead 固定 | 途中で leadership 移転不可 |
| パーミッション | スポーン時に設定。全 teammate が lead のパーミッションモードを継承 |
