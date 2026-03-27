# CLAUDE.md

## リポジトリ概要

macOS/Linux開発環境のdotfiles。シンボリックリンク方式でgit管理。

## 主要コマンド

```bash
./lib/dotfilesLink.sh       # シンボリックリンク作成・再作成
brew bundle --global        # Homebrewパッケージ更新
```

## 技術スタック

- Zsh + Sheldon（プラグイン管理）
- ghostty（ターミナル、Tokyo Night）
- yazi（ファイルマネージャー）+ fazif.yazi（fd/rg + fzf fuzzy検索）
- Claude Code（.claude-global/に集約）

## アーキテクチャ概要

- `.`で始まるファイル → `~`へシンボリックリンク
- `sheldon/`, `ghostty/`, `yazi/`, `starship/` → `~/.config/`配下
- `.claude-global/` → `~/.claude/`配下

### Claude Code設定のレイヤー構造

| 層 | 用途 | 適用タイミング |
|----|------|--------------|
| settings.json | permissions、hooks登録、UI | 常時（機械制御） |
| hooks/ | イベント駆動の自動処理 | イベント発生時 |
| CLAUDE.md | プロジェクト全体の方針 | 常時 |
| rules/ | パス固有のガイドライン | 該当パスのファイルアクセス時 |
| skills/ | ワークフロー（サブエージェント含む） | 明示的呼び出し or キーワードマッチ |

**重要**: `.claude/agents/` は使用しない。サブエージェントはskill内でTask toolを使ってadhocに呼び出す。

### docs/ と rules/ の責務分担

| 観点 | docs/ | rules/ |
|------|-------|--------|
| 役割 | 正（マスター）、詳細リファレンス | Claude向け凝縮ルール |
| 対象読者 | 人間 | Claude |
| 記述スタイル | 詳細・読みやすい | 高シグナル・簡潔 |
| 参照関係 | - | docs/を参照しない（自己完結） |
| 重複 | 許容（用途別最適化優先） | 許容 |

## コーディング原則

- シンプルさ優先（KISS）
- 既存パターンに従う（新規ツールは`.zsh/[tool].zsh`）
- 詳細は各`rules/`ファイルを参照
- 同じ指示を CLAUDE.md と rules/ に重複記載しない

## Gotchas

- **rules の paths 記法**: YAML配列（ハイフン複数行）は適用されない。カンマ区切りワンライナー、クオーテーションなしで記述
  ```yaml
  paths: **/*.test.*, **/*.spec.*
  ```
- **macOS lsof の複数条件**: `-a`フラグ必須（AND条件）。`lsof -a -d cwd -p $pid`
- **コマンド名衝突**: `cc*` 系は既存エイリアス確認必須（例: `ccs`=`claude --model sonnet`）
- **サブエージェント残存**: Task toolで起動したサブエージェントは親セッション終了後も残存しメモリ消費。定期的に`cck --sub`でクリーンアップ
- **zsh サブシェル内 PATH**: `$()` 内でPATH解決失敗の可能性。フルパス使用（例: yaziプラグインで `/opt/homebrew/bin/fd`）
- **zsh NULLCMD の罠**: `> file` だけの行は `cat > file` として実行される（NULLCMD デフォルト）。stdin待ちでハング。解決策: `: > file`
- **Task tool 並列制限**: `run_in_background` + TaskOutput並列取得は全失敗（Sibling error）。foreground で最大5並列が安全上限
- **Task tool 制約**: 孫エージェントスポーン不可（1段階のみ）。PreToolUse/PostToolUse hooks はバイパスされる
- **JSON 出力形式**: `--output-format json` はJSON配列。`jq -r '.[] | select(.type=="result") | .result'` で抽出。`-s` は不要（既に配列）。NDJSONが必要なら `--output-format stream-json`
- **Skill tool 連鎖不可**: Skill tool で他スキルを呼び出すと失敗。スキル間連携はユーザーに次スキルを案内
- **スキル間参照**: 自動読み込みされない。references/ は同一スキル内のみ機能。スキル間参照はデッドリンク化
- **ToolSearch**: claude-haiku-4-5 では利用不可（tool_reference blocks 非対応）
- **スキル内 `!` バッククォート**: インライン実行で失敗時エラー停止。`|| true` や `|| echo "(なし)"` でフォールバック必要
- **macOS BSD awk の match() 3引数制限**: `match(str, /re/, arr)` の3引数形式（キャプチャグループ）は BSD awk 非対応。`grep -oE` や `sed` で代替
- **set -euo pipefail + grep**: grep マッチなしで exit code 1 → スクリプトクラッシュ。`grep パターン || true` で継続