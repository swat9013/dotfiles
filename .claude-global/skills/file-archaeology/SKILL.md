---
name: file-archaeology
description: >-
  gitリポジトリ内のファイルの導入経緯・変遷・設計意図を体系的に調査し、レポートを生成する。
  「ファイルの歴史」「導入経緯」「なぜこのファイルがあるのか」「変更履歴を調べて」
  「file archaeology」「コード考古学」「git調査」と依頼された時に使用。
argument-hint: "<filepath> [調査の動機や着目点]"
user-invocable: true
---

# File Archaeology

gitリポジトリ内のファイルの導入経緯・変遷・設計意図を調査し、markdownレポートを生成する。

## 使用方法

```
/file-archaeology scripts/ci/stop_test_container.sh
/file-archaeology .gitlab-ci.yml CIジョブが間欠的に失敗する原因を追跡
```

## Phase 1: 変更履歴の取得

ファイルの全変更履歴を取得し、調査の方向性を決める。

```bash
git log --all --follow --format="%h %ad %an %s" --date=short -- <file>
```

結果を時系列テーブルにまとめ、以下を特定する:
- **初回コミット**: 誰が、いつ、どんなメッセージで導入したか
- **変更パターン**: 頻繁に変更される箇所、長期安定部分
- **調査クエスチョン**: 「なぜ導入された?」「なぜこの方式に変わった?」など

## Phase 2: 深掘り調査

Phase 1の調査クエスチョンに基づき、技法を選択して深掘りする。

### 技法選択ガイド

| 調査したいこと | 技法 |
|--------------|------|
| 文字列の追加/削除コミット | `git log -S` (pickaxe) |
| パターンの変化追跡 | `git log -G` (regex) |
| 初回導入/MRの詳細 | `git show` + `git log --merges --grep` |
| MRのコミット一覧 | `git log <merge>^2 --not <merge>^1` |
| 行単位の変更者特定 | `git blame -w -M -C -L` |
| 問題導入コミット特定 | `git bisect` |
| mainへのマージ時点 | `git log --first-parent` |
| MRのtitle/description | `./scripts/gitlab/get_mr.sh <IID>` |
| ファイルの初回追加コミット | `git log --diff-filter=A` |
| 2コミット間のマージ経路 | `git log --ancestry-path` |

コマンドの詳細な構文は [references/git-commands.md](references/git-commands.md) を参照。

### 調査の進め方

1. **導入経緯の追跡**: 初回コミットの差分確認 → マージコミット/MR特定 → 前身の実装をpickaxeで遡る
2. **変化の軌跡追跡**: 主要コミットの差分と `--stat` で変更規模を把握 → 動機をコミットメッセージやMRから読み取る
3. **MRコンテキスト補完**: マージコミットメッセージから `See merge request !<IID>` を抽出 → `./scripts/gitlab/get_mr.sh` が存在する場合のみMR情報を取得

調査規模が大きい場合（コミット数が多い、複数ファイルにまたがる等）はサブエージェント（sonnet）で並列化する。個別コミットの差分取得など軽量な収集タスクはhaikuを使用する。

## Phase 3: 統合・レポート作成

[references/report-template.md](references/report-template.md) に従い、レポートを作成する。

**統合時のチェック**:
- 時系列の整合性（日付・コミット順序に矛盾がないか）
- 未解明事項の明示（調査で判明しなかったことを隠さない）
- 使用コマンドの記録（再現可能性の担保）

**出力先**: `tmp/file-archaeology/<filename>-<YYYYMMDD>.md`

## 成功基準

1. ファイルの導入動機が証拠付きで説明されている
2. 主要な変更が時系列で整理されている
3. 使用したgitコマンドが記録され、調査が再現可能
4. 未解明事項が明示されている
5. 調査動機に対する回答・示唆が含まれている
