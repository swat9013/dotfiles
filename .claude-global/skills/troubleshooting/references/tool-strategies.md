# 調査ツール戦略

## エラーメッセージからの逆引き

1. Grep でエラーメッセージをコードベース全体で検索
2. Read で該当箇所の前後50行を確認
3. 呼び出し元を特定（Grep で関数名検索、または Serena `find_referencing_symbols`）

## ログ分析パターン

- 構造化ログ: `Bash("jq 'select(.level==\"ERROR\")' < logfile")`
- 非構造化ログ: `Grep(pattern: "ERROR|WARN|Exception")`
- タイムスタンプ相関: `Grep(pattern: "2026-02-22T1[0-2]")` で時間帯を絞る
- 大量ログ: `Bash("wc -l logfile")` でサイズ確認後、`Read(offset, limit)` で部分読み

## Git履歴からの原因特定

- `git log --oneline -20 -- path/to/file` で最近の変更を確認
- `git blame path/to/file` で行単位の変更者・日時を特定
- `git diff HEAD~5 -- path/to/file` で最近の差分を確認
- `git log --all --oneline -- path/to/deleted-file` で削除ファイルを追跡
- `git bisect` で問題を導入したコミットを二分探索

## シンボルレベル調査（Serena MCP）

大規模コードベースでの深い調査に使用:
- `find_symbol`: 定義場所の特定（名前のサブストリングで検索可能）
- `find_referencing_symbols`: 影響範囲の把握（誰がこのシンボルを使っているか）
- `get_symbols_overview`: ファイル内のシンボル一覧（構造の俯瞰）

## フロントエンド/UI問題

- **表示の問題**: agent-browser の `snapshot` でARIAスナップショット取得
- **操作の問題**: agent-browser の `click`/`fill` で操作を再現
- **ネットワーク/API問題**: agent-browser の `console` でコンソールエラー確認
- **再現テスト**: playwright-cli でE2Eテスト作成（`npx playwright codegen`）

## プロセス/ネットワーク問題

- ポート使用状況: `lsof -a -i :PORT -P`（macOSでは `-a` 必須）
- プロセス確認: `ps aux | grep process-name`
- ディスク/メモリ: `df -h` / `vm_stat`（macOS）
