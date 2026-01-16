---
name: config-optimizer
description: Claude Code設定の最適化と改善提案。「設定を最適化」「config-optimizer」「Claude設定チェック」と依頼された時に使用。skills/commands/agents/settings.json/CLAUDE.mdをベストプラクティスで分析・最適化。
---

# Config Optimizer

Claude Code設定をベストプラクティスに照らして分析・最適化し、改善提案を生成するスキル。

## 最適化対象

| 対象 | パス |
|-----|------|
| グローバルCLAUDE.md | `~/.dotfiles/.claude-global/CLAUDE.md` |
| プロジェクトCLAUDE.md | `.claude/CLAUDE.md`, `CLAUDE.md` |
| settings.json | `~/.dotfiles/.claude-global/settings.json` |
| rules | `~/.dotfiles/.claude-global/rules/`, `.claude/rules/` |
| skills | `~/.dotfiles/.claude-global/skills/`, `.claude/skills/` |
| commands | `~/.dotfiles/.claude-global/commands/`, `.claude/commands/` |
| agents | `~/.dotfiles/.claude-global/agents/`, `.claude/agents/` |

---

## 実行手順

### 1. 対象スキャン

**推奨: スクリプトを使用**:
```bash
# 設定ファイル・ディレクトリの存在確認
~/.dotfiles/.claude/skills/config-optimizer/scripts/scan-targets.sh

# 機械的チェック（行数、構造、frontmatter）
~/.dotfiles/.claude/skills/config-optimizer/scripts/check-config.sh

# Skillsのfrontmatter・行数チェック（詳細）
~/.dotfiles/.claude/skills/config-optimizer/scripts/check-skills.sh
```

または手動で以下のパスを確認し、存在するものをリスト化:
- `~/.dotfiles/.claude-global/CLAUDE.md`
- `~/.dotfiles/.claude-global/settings.json`
- `~/.dotfiles/.claude-global/rules/`
- `~/.dotfiles/.claude-global/skills/`
- `~/.dotfiles/.claude-global/commands/`
- `~/.dotfiles/.claude-global/agents/`
- `.claude/CLAUDE.md`
- `.claude/rules/`
- `.claude/skills/`
- `.claude/commands/`
- `.claude/agents/`
- `CLAUDE.md`

### 2. カテゴリ別分析・最適化

Task toolが利用可能な場合、**6つのサブエージェントを単一メッセージで並列実行**:

| 担当 | チェックリスト |
|------|--------------|
| Rules Optimizer | `./checklists/rule.md` |
| Skills Optimizer | `./checklists/skill.md` |
| Commands Optimizer | `./checklists/command.md` |
| Agents Optimizer | `./checklists/agent.md` |
| Settings Optimizer | `./checklists/settings.md` |
| CLAUDE.md Optimizer | `./checklists/claude-md.md` |

各サブエージェントへのprompt構造:
```
あなたは${カテゴリ}専門のClaude Code設定オプティマイザーです。

## 原則
- ベストプラクティスに照らして問題を特定
- 具体的な改善案を提示
- 優先度を適切に付与（Critical/High/Medium/Low/Info）

## チェックリスト
${チェックリスト内容}

## 最適化対象
${対象ファイルの内容}

## 出力形式
### ${カテゴリ}最適化結果

#### Critical
（なければ「なし」）

#### High
- [ファイルパス] 問題タイトル
  - **問題**: 説明
  - **改善案**: 具体的な修正

#### Medium / Low / Info
（同様）
```

Task tool非対応の場合は6カテゴリを**順次**分析・最適化。

### 3. 結果統合

- 優先度順（Critical→High→Medium→Low→Info）に再編成
- 同一問題への重複指摘を統合
- カテゴリ横断の一貫性問題を追加（例：settings.jsonで参照されているスクリプトが存在しない）

### 4. 結果出力

→ `./templates/config-optimizer-output.md`

### 5. 修正提案

分析結果から修正可能な項目を特定し、ユーザーに提案:

| 項目 | 修正内容 | ファイル |
|------|----------|---------|
| name形式 | ハイフンケースに変換 | SKILL.md |
| frontmatter | テンプレート追加 | SKILL.md |
| $schema | スキーマURL追加 | settings.json |
| deny | 機密ファイル追加 | settings.json |

→ AskUserQuestionで修正可否を確認

### 修正判断基準

各問題タイプに対する修正アクションを定義:

| 問題タイプ | 修正アクション | 対象 | 自動修正可否 |
|-----------|--------------|------|-------------|
| name形式不正 | ハイフンケースに変換 | SKILL.md frontmatter | ✅ |
| description欠落 | テンプレートから生成 | SKILL.md frontmatter | ✅ |
| $schema欠落 | スキーマURL追加 | settings.json | ✅ |
| deny未設定 | 機密ファイルパターン追加 | settings.json | ✅ |
| 行数超過 | 分割提案 | SKILL.md本体 | ⚠️ 提案のみ |
| 構造不備 | ディレクトリ作成提案 | skills/*/references等 | ⚠️ 提案のみ |

詳細な修正テンプレートは `references/fix-templates.md` を参照。

### 6. 修正実行

ユーザー承認後、Edit ツールで修正を適用:
- 各修正項目をEditツールで実行
- 修正完了後、変更サマリーを表示

### 7. Report

修正完了後、Before/After比較を出力:

| メトリクス | Before | After | 変化 |
|-----------|--------|-------|------|
| Critical件数 | X | Y | -Z |
| High件数 | X | Y | -Z |
| 自動修正適用数 | - | N件 | - |

---

## レイヤー選択ガイド

→ 各レイヤーの詳細は `.claude/rules/` を参照:
- `rules/overview.md` - 全体構造
- `rules/skills.md` - skills vs agents vs rules
- `rules/claude-md.md` - CLAUDE.md vs rules

### 最適化時の確認ポイント

1. **Rules vs CLAUDE.md**
   - Rulesに書かれた内容がglobsで絞り込めない場合 → CLAUDE.mdへ移動を提案
   - CLAUDE.mdに書かれた内容がファイルパターンで限定できる場合 → Rulesへ移動を提案

2. **行数制限の遵守**
   → 各rulesに定義された行数制限を参照

3. **重複の排除**
   - 複数のレイヤーに同じ内容がないか確認
   - より適切なレイヤーへの統合を提案

---

## 出力形式

```markdown
# Claude Code 設定最適化結果

## サマリー
| 優先度 | 件数 |
|--------|------|
| Critical | X |
| High | X |
| Medium | X |
| Low | X |
| Info | X |

## Critical
### 1. [Skills] path/to/file - 問題タイトル
**問題**: [具体的な説明]
**改善案**: [修正案]

## High / Medium / Low / Info
（同様の形式）

## 一貫性チェック
- グローバル/プロジェクト矛盾: [状態と詳細]
- Skills/Commands役割分担: [状態と詳細]
- settings.json参照整合性: [状態と詳細]

## 推奨アクション
1. [優先度] [具体的なアクション]
```

---

## 成功基準

1. 全対象ファイル/ディレクトリがスキャンされている
2. 各チェックリストの項目が網羅されている
3. 改善案が具体的で実行可能
4. 優先度が適切に付与されている（Critical: 動作不能・セキュリティリスク、High: 機能低下、Medium: 保守性、Low: スタイル）
5. カテゴリ横断の一貫性チェックが実施されている

---

## ベストプラクティス詳細

→ [config-best-practices.md](../_shared/guides/config-best-practices.md)

---

## 原則

- **具体性**: 「改善すべき」ではなく、具体的な修正案を提示
- **優先度の厳格性**: Critical/Highは実害があるもののみ
- **一貫性**: カテゴリ間の整合性を重視
- **実行可能性**: すぐに実行できる改善案を提供
