# Commands 設計ガイド

## 設計原則

1. **最小権限**: `allowed-tools`で必要なツールのみ許可
2. **動的コンテキスト**: `!` プレフィックスでシェル出力を注入
3. **名前空間**: サブディレクトリで整理

## Skills vs Commands

| 観点 | Skill | Command |
|------|-------|---------|
| 用途 | 対話・相談 | アクション実行 |
| 呼び出し | Claude自動選択 or 明示 | /コマンド名 |
| 例 | code-review, debug | commit, deploy |
