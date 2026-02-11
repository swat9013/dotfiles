---
name: managing-skills
description: Claude Codeのスキルを設計・作成・更新する。frontmatter設計、Progressive Disclosure、自由度設定のガイダンスを提供。「スキルを作って」「スキルを変更」「スキルを更新」「新しいスキルを追加」と依頼された時に使用。
user-invocable: false
---

# Skill Creator

## スキルの構成

- `references/`: 必要時のみ読み込み（トークン節約）
- `scripts/`: 読み込まず実行（出力のみコンテキスト消費）

## 作成フロー

### 1. 要件確認

ユーザーに質問:
- 何を自動化/支援したいか
- どんな言葉で呼び出したいか（トリガー）
- 入力と出力は何か

### 2. スキル初期化

```bash
~/.dotfiles/.claude-global/skills/skill-creator/scripts/init_skill.sh <skill-name> <path>
```

### 3. SKILL.md編集

生成されたテンプレートを編集:
- frontmatterのdescriptionを具体的に
- 手順を簡潔に記述
- 詳細はreferencesに分離

### 4. 検証

- 代表的なタスクでスキルを呼び出し、期待動作を確認
- Claudeがreferencesを適切に参照するか観察
- 不要なファイルを読みすぎていないか確認
- 説明過多な箇所を削除（「Claudeは既にこれを知っているか？」）

## description設計

descriptionはClaude自動選択のトリガー。**三人称で「何をするか + いつ使うか」を書く**。

```yaml
# 悪い例
description: ドキュメントを処理する              # 曖昧
description: ユーザーのPDF処理を手伝う           # 二人称

# 良い例
description: PDFからテキスト・表を抽出し、フォーム入力、文書結合を行う。PDF操作、フォーム記入、文書抽出と依頼された時に使用。
```

**重要**: 一人称・二人称は発見に問題を起こす。三人称で記述すること。

## 設計原則

1. **簡潔に**: SKILL.md本文は500行以下
2. **Claudeは賢い**: 既知の情報は書かない。各情報に「Claudeはこれを知っているか？」と問う
3. **例 > 説明**: 冗長な説明より具体例
4. **Progressive Disclosure**: 詳細はreferencesに（1階層まで）
5. **参照深度制限**: SKILL.md → references/xxx.md（2階層以上は避ける）
6. **用語の一貫性**: 1つの概念に1つの用語。混在させない

## 自由度の設定

スキル本文の記述粒度を、タスクの壊れやすさに合わせる。

| 自由度 | 適用場面 | 記述スタイル |
|--------|---------|-------------|
| 高 | 複数アプローチが有効（レビュー、分析） | 方針・判断基準のみ |
| 中 | 推奨パターンあり、変形も許容 | 擬似コード・パラメータ付きテンプレート |
| 低 | 操作が壊れやすい（deploy、DB移行） | 具体的コマンド・厳密な手順 |

**判断の比喩**: 崖に挟まれた細い橋（低自由度）vs 障害物のない平原（高自由度）

各自由度の具体例: [patterns.md](references/patterns.md)

## 成功基準

1. スキルディレクトリとSKILL.mdが作成されている
2. frontmatterにnameとdescription（トリガー含む）が記載されている
3. 実際にスキルを呼び出して期待する動作が得られる

## 完了チェックリスト

- [ ] スキルディレクトリを作成した
- [ ] SKILL.mdに必須フィールド（name, description）を記載した
- [ ] descriptionが三人称形式で、トリガーキーワードを含む
- [ ] 自由度レベルがタスクの性質に合っている
- [ ] 「Claudeが既に知っている情報」を含めていない
- [ ] 実際に呼び出して期待の動作をすることを確認した

設計パターン詳細: [patterns.md](references/patterns.md)
