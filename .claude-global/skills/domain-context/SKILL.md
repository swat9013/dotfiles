---
name: domain-context
description: プロジェクト固有のドメイン知識を管理。「学びを保存」「ドメイン知識更新」「domain-context」と依頼された時に使用。
---

# Domain Context

プロジェクト固有のドメイン知識を抽出・永続化するスキル。

## 対象

| ファイル | 内容 |
|----------|------|
| プロジェクトCLAUDE.md | アーキテクチャ、技術選択、コーディング規約 |
| docs/decisions/ | ADR（設計判断記録） |
| docs/guidelines/ | 詳細ガイドライン |

※ グローバルCLAUDE.md、settings.json、skills/ は config-optimizer の責務

## モード選択

| モード | 用途 | トリガー |
|--------|------|----------|
| Reflect | セッション会話から自動抽出 | **デフォルト**（`/domain-context`） |
| Manual | 手動で知識を抽出・追記 | ユーザーが明示的に内容を指定 |

## Manual モード

### 1. 抽出

セッション中の学びを特定:

| カテゴリ | 抽出対象 | 保存先 |
|----------|----------|--------|
| アーキテクチャ | 構造、レイヤー、依存関係 | CLAUDE.md `## Architecture` |
| 技術選択 | Why付きの判断 | docs/decisions/ADR-xxx.md |
| コーディング規約 | プロジェクト固有ルール | CLAUDE.md `## Coding Guidelines` |
| 落とし穴 | 実際にハマった問題 | CLAUDE.md `## Gotchas` |
| コマンド | 頻用ワークフロー | CLAUDE.md `## Common Commands` |

### 2. 保存先判断

| 基準 | 保存先 |
|------|--------|
| 1-2行で表現可能 | CLAUDE.md |
| 背景・代替案を含む判断 | docs/decisions/ (ADR形式) |
| 詳細手順・ガイド | docs/guidelines/ |

→ ADRテンプレート: `~/.claude/skills/domain-context/templates/adr.md`

### 3. 追記

- 既存セクションに追記、または新規セクション作成
- 重複確認: 同じ内容が既にないか
- 確認なしで即時追記

## Reflect モード

セッション会話からドメイン知識を自動抽出。

### 1. セッション取得

```bash
# セッションファイルを特定
SESSION=$(~/.dotfiles/.claude-global/skills/domain-context/scripts/find-session.sh)

# メッセージを抽出（Markdown形式）
~/.dotfiles/.claude-global/skills/domain-context/scripts/extract-messages.sh "$SESSION"
```

**対象**: 現在実行中のセッション

### 2. サブエージェントに分析委譲

Task tool で専門エージェントに委譲:

```
あなたはドメイン知識抽出の専門家です。

## 原則
- 具体的で再利用可能な知識のみ抽出
- 汎用的なベストプラクティスは除外
- このプロジェクト固有の学びに集中

## 入力
セッションJSONL: {ファイルパス}
既存CLAUDE.md: {内容}
既存docs/: {構造}

## 抽出カテゴリ
| カテゴリ | 探すべきパターン |
|----------|-----------------|
| アーキテクチャ | 構造に関する発見・確認 |
| 技術選択 | 判断の理由・トレードオフ |
| コーディング規約 | プロジェクト固有ルール |
| 落とし穴 | 実際にハマった問題・エラー |
| コマンド | 有用なワークフロー・スクリプト |

## セッション読み込み
extract-messages.sh の出力（Markdown形式）を使用。
- user/assistant メッセージを抽出済み
- tool_use は【tool: name】形式で表示

## 除外基準
- 一時的な作業メモ
- 汎用的なベストプラクティス
- セッション限りの判断

## 出力
templates/session-reflect-output.md 形式で報告:
- セッション情報
- 抽出結果テーブル（カテゴリ、内容、保存先、確信度）
- 詳細（根拠の引用付き）
```

テンプレート: `~/.claude/skills/domain-context/templates/session-reflect-output.md`

### 3. 整合性チェック

サブエージェント結果を確認:

- 既存 CLAUDE.md との重複
- 既存 docs/ との矛盾
- 確信度の妥当性

### 4. 保存

- 確認なしで即時追記
- 抽出結果を表示後、自動的に保存

## 追記後の行数チェック

追記完了後、CLAUDE.md の行数を確認:

```bash
wc -l ./CLAUDE.md
```

**150行を超えた場合**: `/context-optimizer` を自動呼び出し

## CLAUDE.md テンプレート

新規プロジェクトの場合:

```bash
cp ~/.dotfiles/.claude-global/skills/domain-context/assets/claude-md-template.md ./CLAUDE.md
```

## 成功基準

1. 学びが再利用可能な形で永続化されている
2. 適切な保存先に振り分けられている
3. 既存内容と重複・矛盾がない

## 原則

- **ドメイン知識のみ**: Claude設定は config-optimizer の責務
- **最小限の高シグナル情報**: 効果を最大化
- **具体性**: 抽象論ではなく、このプロジェクト固有の知識
