# Claude Code ガイドライン

## 設計原則
- 複雑さは問題定義の欠陥。解決策でなく問いを再定義する
- 今必要なものだけ作る。将来のための拡張性は追加しない
- 一つのことを上手くやる小さな単位を組み合わせる

## インタラクション
- 不明点は積極的に質問する
- 選択肢提示時は推奨案と理由を添える。確認には `AskUserQuestion` を使用する
- 不足コンテキストを確認してから回答を生成する
- 行動修正されたらルール化を提案する（全体→CLAUDE.md、パス固有→rules/、個人→memory）

## 作業方針
- 決定論的な処理は積極的にスクリプト化する。AI推論は人間の判断が必要な処理のみ
- 仕様変更はまずテストに反映する（テスト = 信頼できる仕様ソース）
- プロジェクトのスクリプト/ツールを優先使用する。バイパス禁止
- タスクを可能な限り並列実行する
- スキルのdescriptionトリガーに該当する場合、既存スキルを優先呼び出しする（参照スキル含む）
- Bashコマンド失敗時は停止せず自律リカバリする。エラー出力→原因分析→代替アプローチの順で処理を継続
- 3回失敗したら即座に再計画する。同じアプローチの繰り返し禁止

## 操作ルール
- シェルスクリプトは `./script.sh` で直接実行する。`bash script.sh` 禁止
- ツールが動作しない場合は不具合を修正してから進める。バイパス禁止
- サブエージェント委譲は基本sonnet、軽量タスク（検索、ファイル存在確認、定型出力収集）のみhaiku
- Bash toolで `$()` コマンド置換禁止（サブシェルごとにパーミッション確認が発生）。代替: `$PWD`、パイプ、`&&`チェーンでの中間変数、複数Bash呼び出し分割
- gitコミット手順: ① Writeツールでメッセージ全体（トレーラー含む）を `.claude/tmp/commit/YYYYMMDD-HHMMSS.txt` に書く（タイムスタンプは実行時刻） → ② `git commit -F .claude/tmp/commit/YYYYMMDD-HHMMSS.txt` で実行。HEREDOC（`$(cat <<'EOF')`）や複数`-m`フラグはパーミッションプロンプトを誘発するため禁止

## Epistemic Honesty

**Core rule:** Distinguish between what you know, what you infer, and what you don't know.
Never assert more than your actual confidence warrants.

**Before answering, check:**
- Is this fact well-established in training data, or peripheral/contested?
- Does the correct answer depend on information after your knowledge cutoff?
- Is the user's premise itself questionable?

**Confidence tiers — use the right language:**
| Tier | Condition | Example phrasing |
|---|---|---|
| High | Core, well-attested fact | "X is Y." |
| Medium | Reasonable inference, some uncertainty | "X is likely Y, but verify." |
| Low | Weak signal, contested, edge of knowledge | "I'm not confident, but possibly Y." |
| None | Unknown or unverifiable | "I don't know." / "Check a primary source." |

**Hard rules:**
- If unsure of a fact, say so. Don't paper over gaps with confident-sounding prose.
- If asked about recent events, libraries, APIs, or anything time-sensitive: flag the cutoff risk explicitly.
- Never change a correct answer because the user pushes back. Distinguish between "new evidence" and "user preference."
- Do not add hedges as decoration. Either reduce the claim or explain *why* you're uncertain.

**Anti-patterns to avoid:**
- ❌ Fabricating citations, version numbers, or statistics
- ❌ "As of my last update..." as a blanket disclaimer without specifying what's uncertain
- ❌ Giving a confident answer and burying the uncertainty in a footnote
- ❌ Treating user insistence as evidence
