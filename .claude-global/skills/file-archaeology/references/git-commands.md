# Git Commands カタログ

## ファイル履歴

### 全変更履歴（リネーム追跡）

```bash
git log --all --follow --format="%h %ad %an %s" --date=short -- <file>
```

`--follow`: リネーム前の履歴も追跡。`--all`: 全ブランチを対象。

### 初回導入を特定

```bash
git log --all --follow --diff-filter=A --format="%h %ad %an %s" --date=short -- <file>
```

`--diff-filter=A`: Added（追加）のみ。他に `D`(削除), `M`(変更), `R`(リネーム)。

### mainへのマージ時点

```bash
git log --first-parent --format="%h %ad %an %s" --date=short -- <file>
```

`--first-parent`: マージコミットの第一親のみ辿り、mainline上の変更点を表示。

### 変更者の統計

```bash
git shortlog -sn --all -- <file>
```

誰が何回変更したかの概要。オーナーシップの把握に。

## コミット詳細

### コミットの差分を確認

```bash
git show <commit> --format="%H %ad %an%n%B" --date=short -- <file>
```

特定ファイルに限定した差分を表示。`%B`: コミットメッセージ全文。

### 変更規模の概要

```bash
git show <commit> --stat
```

### 複数コミットの一括概要

```bash
for h in <hash1> <hash2> ...; do
  echo "=== $h ==="; git show "$h" --format="%h %ad %an %s" --date=short --stat | head -15; echo
done
```

## 文字列追跡

### 文字列の追加/削除コミット（pickaxe）

```bash
git log --all -S "<string>" --format="%h %ad %an %s" --date=short -- <file>
```

`-S`: 文字列の出現数が変化したコミットを検出。導入・削除の追跡に最適。
`-- <file>` は省略可（全ファイル対象）。

### 正規表現パターンの変化

```bash
git log --all -G "<regex>" --format="%h %ad %an %s" --date=short -- <file>
```

`-G`: パターンにマッチする行が変化したコミットを検出。
`-S` との違い: `-S` は出現数の変化、`-G` は行内容の変化。

### pickaxe + 正規表現

```bash
git log --all -S "<regex>" --pickaxe-regex --format="%h %ad %an %s" --date=short
```

`-S` に正規表現を使いたい場合。

## ブランチ/MR追跡

### マージコミットの検索

```bash
git log --all --merges --oneline --grep="<branch-name>"
```

ブランチ名からマージコミットを特定。MR IIDはマージコミットメッセージから抽出可能。

### MRブランチのコミット一覧

```bash
git log --format="%h %ad %an %s" --date=short <merge>^2 --not <merge>^1
```

`<merge>^2`: マージされた側の親。`--not <merge>^1`: main側を除外。

### 2コミット間の経路

```bash
git log --ancestry-path <ancestor>..<descendant> --format="%h %ad %an %s" --date=short
```

特定のコミットがどの経路でmainに到達したかを追跡。

### MR情報の取得（プロジェクト固有）

```bash
# scripts/gitlab/get_mr.sh が利用可能な場合
./scripts/gitlab/get_mr.sh <IID> --fields title,description,labels
```

マージコミットメッセージから `See merge request !<IID>` を抽出してIIDを得る。

## blame

### 行単位の変更者特定

```bash
git blame -w -M -C -L <start>,<end> <file>
```

- `-w`: 空白変更を無視
- `-M`: ファイル内の行移動を検出
- `-C`: ファイル間のコピーを検出
- `-L`: 行範囲を指定

### 特定リビジョンでのblame

```bash
git blame -w -M -C <commit>^ -- <file>
```

指定コミットの直前の状態でblame。「この変更の前は誰が書いたか」を追跡。

## bisect

### 手動bisect

```bash
git bisect start
git bisect bad <bad-commit>
git bisect good <good-commit>
# テスト → git bisect good/bad を繰り返す
git bisect reset
```

### 自動bisect

```bash
git bisect start <bad> <good>
git bisect run <test-command>
git bisect reset
```

`<test-command>` が exit 0 で good、non-zero で bad と判定。
