# Requirements Specification (Full Mode)

## 1. Basic Information

**Project/Feature**: [プロジェクト/機能名]
**Created**: YYYY-MM-DD
**Last Updated**: YYYY-MM-DD
**Status**: [Draft | In Review | Approved | Implemented]
**Owner**: [担当者]
**Stakeholders**: [関係者リスト]

---

## 2. Background and Purpose (5W1H)

### Why (なぜ)
[この要件が必要な理由、解決すべきビジネス課題]

### What (何を)
[何を達成したいか、期待される成果]

### Who (誰が)
[対象ユーザー、ステークホルダー]

### When (いつ)
[期限、マイルストーン]

### Where (どこで)
[適用範囲、環境、プラットフォーム]

### How (どうやって)
[実現手段の大枠、アプローチの方向性]

---

## 3. Problem Definition

### 3.1 XY Problem Check

**本当に解決すべき問題は何か？**

| 質問 | 回答 |
|------|------|
| 思いついた解決策（X）は？ | [例: 「○○ライブラリを導入したい」] |
| その解決策で何を実現したいか（Y）？ | [例: 「データ処理を高速化したい」] |
| なぜそれを実現したいか（根本的な問題）？ | [例: 「ユーザーの待ち時間が長すぎる」] |

### 3.2 Root Cause Analysis (5 Whys)

1. **Why 1**: [表層的な問題]
2. **Why 2**: [一段深い原因]
3. **Why 3**: [さらに深い原因]
4. **Why 4**: [構造的な原因]
5. **Why 5**: [根本原因]

**根本原因**: [最終的に特定された根本原因]

---

## 4. Personas and User Stories

### 4.1 Personas

#### Persona 1: [ペルソナ名]

- **Name**: [名前]
- **Role**: [役割]
- **Goals**: [達成したいこと]
- **Pain Points**: [困っていること]
- **Behavior Patterns**: [行動パターン]
- **Quote**: [代表的な発言]

#### Persona 2: [ペルソナ名]

- **Name**: [名前]
- **Role**: [役割]
- **Goals**: [達成したいこと]
- **Pain Points**: [困っていること]
- **Behavior Patterns**: [行動パターン]
- **Quote**: [代表的な発言]

### 4.2 Jobs to Be Done (JTBD)

**When** [状況・コンテキスト]
**I want to** [実現したいこと]
**So I can** [最終的な成果・利益]

### 4.3 User Stories

#### Epic 1: [大きな機能カテゴリ]

**Story 1.1**: As a [user type], I want [action] so that [benefit].

- **Acceptance Criteria**:
  - Given [前提条件]
  - When [アクション]
  - Then [期待される結果]

**Story 1.2**: As a [user type], I want [action] so that [benefit].

- **Acceptance Criteria**:
  - Given [前提条件]
  - When [アクション]
  - Then [期待される結果]

---

## 5. Success Criteria

### 5.1 Measurable Success Metrics

| Metric | Baseline | Target | Measurement Method |
|--------|----------|--------|-------------------|
| [指標1] | [現状値] | [目標値] | [測定方法] |
| [指標2] | [現状値] | [目標値] | [測定方法] |
| [指標3] | [現状値] | [目標値] | [測定方法] |

### 5.2 Key Performance Indicators (KPI)

- **KPI 1**: [指標名] - [目標値]
- **KPI 2**: [指標名] - [目標値]
- **KPI 3**: [指標名] - [目標値]

---

## 6. Scope and Constraints

### 6.1 In Scope (やること)

- [ ] [実装する機能1]
- [ ] [実装する機能2]
- [ ] [実装する機能3]

### 6.2 Out of Scope (やらないこと)

- [ ] [除外する機能1]
- [ ] [除外する機能2]
- [ ] [除外する機能3]

### 6.3 MoSCoW Prioritization

#### Must Have (M)
- [ ] [必須要件1] - プロジェクト成功に不可欠
- [ ] [必須要件2] - プロジェクト成功に不可欠

#### Should Have (S)
- [ ] [重要要件1] - 重要だが必須ではない
- [ ] [重要要件2] - 重要だが必須ではない

#### Could Have (C)
- [ ] [あると良い要件1] - インパクトは少ない
- [ ] [あると良い要件2] - インパクトは少ない

#### Will Not Have (W)
- [ ] [今回は優先しない要件1]
- [ ] [今回は優先しない要件2]

### 6.4 Constraints

- **Technical**: [技術的制約]
- **Budget**: [予算制約]
- **Timeline**: [時間制約]
- **Resources**: [リソース制約]
- **Regulatory**: [規制・コンプライアンス]

---

## 7. Prioritization

### 7.1 RICE Scoring (Optional)

| Feature | Reach | Impact | Confidence | Effort | RICE Score |
|---------|-------|--------|-----------|--------|-----------|
| [機能1] | [0-10] | [0-3] | [0-100%] | [工数] | [(R×I×C)/E] |
| [機能2] | [0-10] | [0-3] | [0-100%] | [工数] | [(R×I×C)/E] |

### 7.2 Value vs. Complexity Matrix

```
高価値・低複雑度 | 高価値・高複雑度
  [機能A]        |  [機能B]
-----------------+-----------------
低価値・低複雑度 | 低価値・高複雑度
  [機能C]        |  [機能D]
```

---

## 8. Risks and Assumptions

### 8.1 Risks

| Risk | Impact | Probability | Mitigation Strategy |
|------|--------|------------|---------------------|
| [リスク1] | [High/Medium/Low] | [High/Medium/Low] | [対策] |
| [リスク2] | [High/Medium/Low] | [High/Medium/Low] | [対策] |

### 8.2 Assumptions

- [ ] [前提条件1]
- [ ] [前提条件2]
- [ ] [前提条件3]

### 8.3 Dependencies

- [ ] [依存する機能/システム1]
- [ ] [依存する機能/システム2]

---

## 9. Acceptance Criteria

### 9.1 Functional Requirements

#### Requirement 1: [要件名]

- **Given**: [前提条件]
- **When**: [アクション]
- **Then**: [期待される結果]

#### Requirement 2: [要件名]

- **Given**: [前提条件]
- **When**: [アクション]
- **Then**: [期待される結果]

### 9.2 Non-Functional Requirements

#### Performance
- [ ] ページロード時間: [目標値] 秒以内
- [ ] 処理時間: [目標値] ms以内
- [ ] スループット: [目標値] req/sec

#### Security
- [ ] データ暗号化: [暗号化方式]
- [ ] 認証: [認証方式]
- [ ] アクセス制御: [制御方法]

#### Usability
- [ ] 直感的なUI: [基準]
- [ ] アクセシビリティ: [準拠基準（例: WCAG 2.1 AA）]
- [ ] レスポンシブ対応: [対応デバイス]

#### Scalability
- [ ] 同時接続数: [目標値] ユーザー
- [ ] データ増加対応: [対応規模]

#### Reliability
- [ ] 可用性: [目標値]%
- [ ] MTBF: [目標値] 時間
- [ ] MTTR: [目標値] 時間

#### Maintainability
- [ ] コードカバレッジ: [目標値]%
- [ ] 技術的負債: [許容範囲]
- [ ] ドキュメント: [必要なドキュメント]

---

## 10. Next Steps

### 10.1 Technical Research
- [ ] [調査項目1]
- [ ] [調査項目2]

### 10.2 Design & Architecture
- [ ] [設計タスク1]
- [ ] [設計タスク2]

### 10.3 Prototyping & Validation
- [ ] [プロトタイプ作成]
- [ ] [ユーザビリティテスト]

### 10.4 Implementation Planning
- [ ] [タスク分解]
- [ ] [実装計画作成]
