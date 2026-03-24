---
name: mermaid-syntax
description: Guides Mermaid diagram syntax safety rules and type selection. Use when「Mermaid」「シーケンス図」「フローチャート」「ER図」「状態遷移図」。
user-invocable: false
---

# Mermaid Syntax Skill

Mermaidダイアグラム生成時に適用する構文安全ルールと図種選択ガイド。

## 構文安全ルール（必須）

### 1. ASCII限定

| NG | OK | 理由 |
|----|----|------|
| スマートクォート `""` | ストレートクォート `""` | パーサー非対応 |
| Unicode矢印 → ← | ASCII矢印 `-->` `<--` | パーサー非対応 |
| Emダッシュ — | ハイフン `-` `--` | パーサー非対応 |

### 2. 1行に1ステートメント

```
# 非推奨（可読性低下のため避ける）
A --> B --> C

# OK
A --> B
B --> C
```

### 3. 特殊文字はHTMLエンティティ

| 文字 | エンティティ | 用途 |
|------|------------|------|
| `"` | `&quot;` | ラベル内引用符 |
| `#` | `&#35;` | ハッシュ記号 |
| `;` | `&#59;` | セミコロン（sequence図） |
| 改行 | `<br/>` | ラベル内改行（`\n`は非対応） |

### 4. 予約語・ID制約

| NG | OK | 理由 |
|----|----|------|
| ノードラベル `end` | `End` / `["end"]` | サブグラフ終端と衝突 |
| ノードID `o`始まり | `output` / `o_dev` | v10以前で`o--`が矢印誤解釈。現行版では改善済みだが安全のため避ける |
| ノードID `x`始まり | `xData` / `x_val` | v10以前で`x--`が矢印誤解釈。現行版では改善済みだが安全のため避ける |

### 5. サブグラフ

- 空のサブグラフ禁止（最低1ノード必須）
- `direction`はサブグラフ先頭に記述
- ネストは2階層まで

### 6. 日本語テキスト

ダブルクォートで囲む: `A["日本語ラベル"]`

### 7. 複雑さ上限

| 項目 | 上限 |
|------|------|
| Flowchartノード | 30 |
| Sequence参加者 | 8 |
| ラベル文字数 | 25 |
| Pieセクション | 7 |
| サブグラフネスト | 2階層 |

上限を超える場合は図を分割する。

## 図種選択

ユースケースに最適な図種を選択する。

### ソフトウェア設計

| ユースケース | 推奨図種 | 代替 |
|------------|---------|------|
| プロセスフロー | `flowchart` | - |
| API連携・メッセージング | `sequenceDiagram` | `zenuml` |
| クラス・ドメインモデル | `classDiagram` | - |
| 状態遷移 | `stateDiagram-v2` | `flowchart` |
| DBスキーマ | `erDiagram` | - |
| システム構成 | `architecture-beta` | `flowchart`, `block-beta` |
| Gitブランチ戦略 | `gitGraph` | - |

### プロジェクト管理

| ユースケース | 推奨図種 |
|------------|---------|
| スケジュール | `gantt` |
| タスク管理 | `kanban` |
| UX体験フロー | `journey` |
| 年表・履歴 | `timeline` |

### データ可視化

| ユースケース | 推奨図種 |
|------------|---------|
| 比率・割合 | `pie` |
| 2軸分析 | `quadrantChart` |
| 折れ線・散布 | `xychart-beta` |
| フロー量 | `sankey-beta` |
| 階層整理 | `mindmap` |

各図種の構文テンプレートは `references/diagram-templates.md` を参照。

## バリデーション

生成後、[Mermaid Live Editor](https://mermaid.live) での検証を推奨。
レンダリングエラー時は、エラーメッセージを基に修正する。
