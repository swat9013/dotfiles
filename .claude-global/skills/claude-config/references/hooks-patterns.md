# Hooks 設計パターン・アンチパターン

## 4つの Hook パターン

| パターン | イベント | 目的 | 例 |
|---------|---------|------|-----|
| Safety Gates | PreToolUse | 危険操作をブロック | `rm -rf`防止、`.env`編集禁止、リンター設定保護 |
| Quality Loops | PostToolUse | 編集のたびにリント→自己修正 | 自動修正→残違反を additionalContext 注入 |
| Completion Gates | Stop | テスト通過まで完了させない | `stop_hook_active` で無限ループ防止必須 |
| Observability | 全イベント | 監視パイプラインにイベント流す | ログ集約、メトリクス収集 |

**Quality Loop の要点**: 自動修正を先に実行し、残った違反だけを `additionalContext` でフィードバック。

**エラーメッセージ構造化**: hook出力を修正指示として設計する — WHY（違反理由+ADR/ルール参照）、FIX（具体的修正手順）、EXAMPLE（Bad→Good）。生のlinter出力より修正精度が向上する。

**リンター設定保護**: エージェントがリントエラー時にコード修正でなく設定変更するのを防ぐ。PreToolUse で `.eslintrc`, `biome.json`, `pyproject.toml` 等への書き込みを exit 2 でブロック。

---

## フィードバック速度階層

チェックを可能な限り速いレイヤーに移動させる:

| レイヤー | 速度 | 手段 |
|---------|------|------|
| PostToolUse Hook | ミリ秒 | 自動フォーマット + リント |
| プリコミットフック | 秒 | Lefthook 等でコミット前検出 |
| CI/CD | 分 | マージ前検出 |
| 人間レビュー | 時間〜日 | マージ後検出 |

**エスカレーション原則**: 同一違反3回繰り返し → 次のエンフォースメントレベルに昇格（L1 ドキュメント指示 → L2 AI検証 → L3 ツール自動検証 → L4 構造テスト/CI）。

---

## アンチパターン

| パターン | 問題 | 対策 |
|---------|------|------|
| Stop フックで `stop_hook_active` チェックなし | 無限ループ | 先頭で必ず確認 |
| exit 2 + stdout JSON の混在 | stdout は無視される | exit 2 → stderr、exit 0 → hookSpecificOutput |
| `decision`/`reason` 旧形式 | 廃止予定 | `hookSpecificOutput.permissionDecision` に移行 |
| PreToolUse で重い処理 | 全ツール呼び出しに遅延 | PostToolUse または `async: true` |
| 相対パス使用 | ディレクトリ移動で壊れる | `$CLAUDE_PROJECT_DIR` を使う |
| シェル変数をクォートなしで展開 | スペース含むパスで破損 | `"$FILE_PATH"` と必ずクォート |
| CLAUDE.md に「リンター実行せよ」と書くだけ | 「ほぼ毎回」止まり | Hook で「例外なく毎回」に強制 |
| `set -e` をhookスクリプトで使用 | jq失敗等で即死→「hook returned error」 | `|| exit 0` で個別ガード |
| `echo "$INPUT"` でJSON受け渡し | 巨大JSON・バックスラッシュで破損 | `printf '%s\n' "$INPUT"` を使う |
