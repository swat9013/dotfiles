# Skills リファレンス

## TOC
- [配置場所と優先度](#配置場所と優先度)
- [Skills vs Commands](#skills-vs-commands)
- [skills vs agents vs rules](#skills-vs-agents-vs-rules)
- [context: fork の agent 値](#context-fork-の-agent-値)
- [skills フィールド（サブエージェントへのプリロード）](#skills-フィールド)
- [Skill(name *) 権限制御](#skill-name--権限制御)
- [フィードバックループパターン](#フィードバックループパターン)
- [トラブルシューティング](#トラブルシューティング)

---

## 配置場所と優先度

| 場所 | パス | 優先度 |
|------|------|--------|
| Enterprise | 組織管理設定 | 1（最高） |
| Personal | `~/.claude/skills/` | 2 |
| Project | `.claude/skills/` | 3 |
| Plugin | `plugin-name:skill-name` | 4（名前空間分離） |

- 同名スキルは優先度の高い方が使用される
- skills と commands が同名の場合、**skills が優先**
- `.claude/commands/` は後方互換性あり、skills への移行を推奨

---

## Skills vs Commands

| 観点 | Skill | Command |
|------|-------|---------|
| 用途 | 対話・相談・ワークフロー | アクション実行（定型プロンプト） |
| 呼び出し | Claude 自動選択 or `/skill-name` | `/コマンド名`（明示のみ） |
| 構造 | ディレクトリ（複数ファイル可） | 単一ファイル |
| 例 | code-review, debug | commit, deploy |

新規作成は skills を推奨（上位互換）。

---

## skills vs agents vs rules

| 観点 | skills/ | agents/ | rules/ |
|------|---------|---------|--------|
| 用途 | ワークフロー・手順定義、知識・参照情報 | カスタムサブエージェント | パス固有のルール |
| 実行形態 | 対話型・相談型 | コンテキスト分離実行 | 自動適用 |
| トリガー | Claude 自動選択 or `/skill-name` | Task tool 経由 | ディレクトリアクセス時 |
| コンテキスト | 共有（会話継続） | 分離（独立実行） | - |
| 適用場面 | 手順定義、ガイド | 並列実行・試行錯誤・専門役割 | - |

---

## context: fork の agent 値

```yaml
context: fork
agent: Explore   # 探索・調査系
# agent: Plan    # 計画・設計系
# agent: general-purpose  # 汎用
```

- `context: fork` でサブエージェントとして隔離実行
- SKILL.md の内容がサブエージェントへのプロンプトになる
- `agent` フィールドで種別を指定（省略時は general-purpose）

動的コンテキスト注入との組み合わせ例:

```yaml
---
name: pr-summary
context: fork
agent: Explore
---
## PR context
- PR diff: !`gh pr diff`
- Changed files: !`gh pr diff --name-only`

Summarize this pull request...
```

---

## skills フィールド

サブエージェント（`.claude/agents/` または `context: fork`）の frontmatter に `skills:` を指定すると、起動時にスキル内容をコンテキストへ注入できる:

```yaml
---
name: api-developer
description: チーム規約に従い API エンドポイントを実装する。
skills:
  - api-conventions
  - error-handling-patterns
---
```

- 指定スキルの**完全な内容**がサブエージェント起動時にコンテキストへ注入
- サブエージェントは親セッションのスキルを**継承しない** — 必要なスキルは明示指定
- 「利用可能にする」ではなく「実際にコンテキストへロード」する点に注意

---

## Skill(name *) 権限制御

`settings.json` の `permissions` で特定スキルへのアクセスを制御:

```json
{
  "permissions": {
    "allow": ["Skill(commit)", "Skill(review-pr *)"],
    "deny": ["Skill(deploy *)"]
  }
}
```

- `Skill(name)` — 特定スキルを許可/拒否
- `Skill(name *)` — ワイルドカードで複数スキルを対象
- `deny` が `allow` より優先（通常の permission ルールと同様）

---

## フィードバックループパターン

品質向上には「実行 → 検証 → 修正 → 繰り返し」パターンが効果的:

```markdown
## ドキュメント編集プロセス

1. 編集を実行
2. **即座に検証**: `python scripts/validate.py`
3. 検証失敗時:
   - エラーメッセージを確認
   - 問題を修正
   - 再度検証
4. **検証パスまで繰り返し**
5. 出力を生成
```

検証ループにより早期にエラーを検出できる。スキル内に明示的なチェックループを記述しておくと Claude が自律的に品質を保つ。

---

## トラブルシューティング

### スキルが発動しない
1. description にユーザーが使う言葉を含めているか確認
2. `What skills are available?` でスキル一覧を確認
3. 直接 `/skill-name` で呼び出してテスト

### スキルが頻繁に誤発動する
1. description をより具体的に
2. `disable-model-invocation: true` で手動呼び出しのみに制限

### スキルが読み込まれない
- skills description は **15,000 文字の budget 制限**あり
- `/context` で除外されたスキルを確認
- `SLASH_COMMAND_TOOL_CHAR_BUDGET` 環境変数で上限を調整可能
