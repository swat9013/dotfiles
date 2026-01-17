---
paths: .claude-global/agents/**
---

# Agents ガイド

## Agents とは

Task tool経由で起動する独立サブエージェント。コンテキスト分離で並列実行・専門役割を実現。

## agents vs skills

| 判断基準 | agents | skills |
|---------|--------|--------|
| 並列実行 | Yes | No |
| 試行錯誤・実験 | Yes | No |
| コンテキスト | 分離 | 共有 |
| 対話継続 | No | Yes |

## ファイル形式

```yaml
---
name: agent-name
description: エージェントの説明
tools:
  - Read
  - Glob
  - Grep
allowedCommands:    # オプション
  - npm test
---

# エージェント名

## ロール
## 原則
## 成功基準
## 出力形式
```

## ツール制限パターン

| パターン | tools | 用途 |
|---------|-------|------|
| 読み取り専用 | Read, Glob, Grep | レビュー・分析 |
| テスト実行 | + Bash (allowedCommands) | テスト・検証 |
| 編集可能 | + Edit, Write | リファクタ（慎重に） |

## 避けるべき内容

- 同一ファイルへの並列編集
- ロール曖昧なエージェント
- 広すぎるツール許可
- 成功基準なし
