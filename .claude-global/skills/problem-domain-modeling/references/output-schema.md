# output-schema.md — モデルファイル スキーマ定義

## 目次

- [frontmatter](#frontmatter)
- [セクション構成](#セクション構成ループごとに繰り返すセクション種別数-5)
  - 語彙採取 / モデル（関係ネットワーク）/ モデル図 / 歪み記録（5種類：MISSING・CONFLICT・AMBIGUOUS・STANDARD-GAP・NAMING-RISK）/ 次ループへの引き継ぎ
  - NAMING-RISK で点検する6パターン
- [終了セクション](#終了セクションstatus-closed-時のみ追記)
- [サンプル出力](#サンプル出力トピックチーム内コミュニケーション摩擦)

---

書き出し先: `.claude/domain-models/YYYY-MM-DD-HHMMSS-{topic}.md`

関連成果物（同ディレクトリ）:
- drawio XML: `YYYY-MM-DD-HHMMSS-{topic}-loopN.drawio`
- PNG エクスポート: `YYYY-MM-DD-HHMMSS-{topic}-loopN.drawio.png`

---

## frontmatter（フィールド数: 4）

```yaml
---
topic: <課題の概要（$ARGUMENTS から生成）>
date: YYYY-MM-DD HH:mm
status: open  # open | closed
loop_count: 0  # ループ完了回数
---
```

---

## セクション構成（ループごとに繰り返す）（セクション種別数: 5）

**## ループN — 語彙採取**
各行: `- **語彙**: ユーザーの文脈説明（原文に近い形で）`
原則: 定義を加えず、使用文脈と共に採取する。

**## ループN — モデル（関係ネットワーク）**
各行: `- A [関係種別] B`
関係種別例: 含む、引き起こす、依存する、競合する、前提とする
原則: 語彙の意味は他語彙との差異関係で定まる。差異が曖昧ならエッジを引かず歪みとして記録する。

**## ループN — モデル図**
drawio 生成物へのリンクを記載する。
```markdown
![モデル図 ループN](YYYY-MM-DD-HHMMSS-{topic}-loopN.drawio.png)

[drawio編集用](YYYY-MM-DD-HHMMSS-{topic}-loopN.drawio)
```

**## ループN — 歪み記録**
各行: `- [歪み種別] 説明 (auto|user)`
末尾の `auto`/`user` は訂正責務の区分。`auto` は次ループで自律訂正、`user` はユーザー判断待ち。
歪み種別（5種類）:

| 種別 | 意味 | 例 |
|------|------|-----|
| MISSING | 欠落 | `[MISSING] 「返信が来ない」の原因（多忙 vs 意図的無視）が未分類 (user)` |
| CONFLICT | 矛盾 | `[CONFLICT] 「非同期推奨」と「即レス文化」が並立している (user)` |
| AMBIGUOUS | 曖昧 | `[AMBIGUOUS] 「心理的安全性」が原因か結果か不明 (user)` |
| STANDARD-GAP | 業界標準語彙との差分（差分確認のみ、補正しない） | `[STANDARD-GAP] 「返信待ち滞留」はDDD語彙に類似なし → 組織固有概念として保持推奨 (auto)` |
| NAMING-RISK | 陳腐化リスクのある命名 | `[NAMING-RISK] 「Slack滞留」は採用技術名依存 → 「非同期チャネル応答遅延」への抽象化候補 (auto)` |

NAMING-RISK で点検する6パターン:

| パターン | 例 | 抽象化候補 |
|---------|-----|-----------|
| 採用技術名 | Slack / Kubernetes / React | 非同期チャネル / コンテナオーケストレーション |
| 製品・ブランド名 | Jira チケット / Figma コメント | 作業項目 / 設計レビューコメント |
| 役職・部署名 | PM承認 / インフラ部対応 | 意思決定者承認 / 基盤運用担当 |
| 固有名詞（人名） | 田中さんが対応 | 担当者〈ロール名〉が対応 |
| 解決策混入 | レビューbot不在 | レビュー遅延 |
| 略語・コードネーム | PROJ-X / FY26-Q1案件 | 〈正式名〉 |

**## ループN — 次ループへの引き継ぎ**
各行: `- 引き継ぎ内容`
ループ2以降の重点例: 暗黙語彙採取（資料に書かれていない日常語）、標準語彙との差分確認、命名リスク箇所の再定義。

---

## 終了セクション（status: closed 時のみ追記）

**## セッションサマリ**
```
採取語彙数: N
モデル化した関係数: N
未解決の歪み: N件（内訳: MISSING=N, CONFLICT=N, AMBIGUOUS=N, STANDARD-GAP=N, NAMING-RISK=N）
完了ループ数: N
成果物:
  - モデル: .claude/domain-models/YYYY-MM-DD-HHMMSS-{topic}.md
  - 最終図: .claude/domain-models/YYYY-MM-DD-HHMMSS-{topic}-loopN.drawio.png
```

---

## サンプル出力（トピック: チーム内コミュニケーション摩擦）

```markdown
---
topic: チーム内コミュニケーション摩擦
date: 2026-04-16 10:00
status: open
loop_count: 1
---

## ループ1 — 語彙採取

- **非同期連絡**: Slackで送っても返信が来ないことが多い
- **会議依存**: 決定はほぼ会議でしか行われない
- **心理的安全性**: 発言をためらう雰囲気がある

## ループ1 — モデル（関係ネットワーク）

- 非同期連絡 [引き起こす] 返信待ち滞留
- 返信待ち滞留 [引き起こす] 会議依存
- 心理的安全性の低下 [依存する] 会議依存

## ループ1 — モデル図

![モデル図 ループ1](2026-04-16-100000-team-comm-friction-loop1.drawio.png)

[drawio編集用](2026-04-16-100000-team-comm-friction-loop1.drawio)

## ループ1 — 歪み記録

- [MISSING] 「返信が来ない」の原因（多忙 vs 意図的無視）が未分類 (user)
- [AMBIGUOUS] 「心理的安全性」の低さが原因か結果か不明 (user)
- [NAMING-RISK] 「Slackで送っても」は採用技術名依存 → 「非同期チャネルで送っても」への抽象化候補 (auto)
- [STANDARD-GAP] 「返信待ち滞留」はDDD/標準語彙に類似なし → 組織固有概念として保持推奨 (auto)

## ループ1 — 次ループへの引き継ぎ

- 「返信しない理由」を語彙として採取する
- 会議依存と非同期不全の因果方向を確認する
- 資料に書かれていない日常語（暗黙語彙）を採取する
```

---

```markdown
---
topic: チーム内コミュニケーション摩擦
date: 2026-04-16 10:30
status: closed
loop_count: 1
---

（... ループ1の5セクション ...）

## セッションサマリ

採取語彙数: 3
モデル化した関係数: 3
未解決の歪み: 4件（内訳: MISSING=1, CONFLICT=0, AMBIGUOUS=1, STANDARD-GAP=1, NAMING-RISK=1）
完了ループ数: 1
成果物:
  - モデル: .claude/domain-models/2026-04-16-100000-team-comm-friction.md
  - 最終図: .claude/domain-models/2026-04-16-100000-team-comm-friction-loop1.drawio.png
```
