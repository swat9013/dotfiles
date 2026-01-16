# Skills チェックリスト

## スコープ

Claude Code Skillsの構造、frontmatter、Progressive Disclosure原則、トリガー明確さを検証。

---

## Critical

### frontmatter必須項目
- [ ] `name` が存在するか
- [ ] `description` が存在するか
- [ ] `name` がハイフンケースで64文字以内か

---

## High

### トリガー明確さ
- [ ] `description` にトリガーキーワードを含むか（「〜と依頼された時に使用」形式）
- [ ] トリガーキーワードが具体的か（曖昧な「処理」「実行」を避ける）
- [ ] 複数のトリガーパターンを提示しているか

### 行数制限
- [ ] SKILL.md本文が500行以下か
- [ ] 500行超の場合、`references/`への分離が適切か

---

## Medium

### Progressive Disclosure
- [ ] 詳細なAPI仕様やスキーマは`references/`に分離されているか
- [ ] `references/`内のファイルが適切に参照されているか（`→` 記法）
- [ ] 深いネスト参照（A→B→C）がないか

### 構造
- [ ] 適切なサブディレクトリ構成（`references/`, `scripts/`, `assets/`）
- [ ] `scripts/`は参照ではなく実行用か
- [ ] セクション分けが明確か（前提条件、実行手順、出力形式、成功基準）

### 冗長性
- [ ] Claudeが既知の一般知識を重複説明していないか
- [ ] 説明より例が優先されているか
- [ ] 同じ内容が複数箇所に記載されていないか

---

## Low

### スタイル
- [ ] 見出しレベルが一貫しているか
- [ ] 表形式の活用（長い箇条書きを避ける）
- [ ] チェックリストの提供（該当する場合）

### optional frontmatter
- [ ] `allowed-tools`は適切に制限されているか
- [ ] `model`指定が必要な場合に設定されているか
- [ ] `context: fork`の使用が妥当か
