---
name: critical-think
description: 前回回答の厳密批評と改善。「批判的に見て」「自己レビュー」「回答を見直して」「クリティカルシンキング」「穴を探して」「弱点ない？」「ツッコミ入れて」「本当に正しい？」と依頼された時に使用。6セクション分析で論理的欠陥を特定し改善版を提示。
disable-model-invocation: true
---

# Critical Think

前回の回答を厳密に批評し、改善版を提供するスキル。

## 分析フレームワーク

以下の6セクションで順番に分析。各セクションを深く分析してから次に進む。

---

### 1. Core Thesis & Confidence Score (Initial)

- **1-1. Core Thesis**: 前回の回答の中心的な解決策・主張を1文で要約
- **1-2. Initial Confidence**: 生成時点での確信度を1-10で評価

---

### 2. Foundational Analysis: Assumptions & Context

- **2-1. High-Impact Assumptions**:
  もし誤りだと判明した場合、提案を完全に無効にする重大な仮定トップ3。
  技術的、環境的、リソース的な仮定に焦点。

- **2-2. Contextual Integrity**:
  会話で言及された制約・要件をすべて尊重したか？
  潜在的な矛盾や忘れられた詳細を指摘。

---

### 3. Logical Integrity Analysis

- **3-1. Premise Identification**:
  議論の基本的な前提は何か？
  （例: 「ユーザーはスケーラブルなソリューションを必要としている」「Redisはレート制限に最適」）

- **3-2. Chain of Inference**:
  前提から最終結論への論理的な連鎖が明確か？
  論理的な飛躍、ギャップ、結論が証拠から必然的に導かれないステップを指摘。

- **3-3. Potential Fallacies**:
  推論に一般的な論理的誤謬が含まれているか？
  （例: 偽の二分法、早まった一般化、疑わしい権威への訴え）

---

### 4. AI-Specific Pitfall Analysis

以下の一般的な失敗モードに対して「Pass」または「Fail」を評価。「Fail」には簡潔な理由を付記。

- **4-1. Problem Evasion**: (Pass/Fail)
  ユーザーの表面的な問題は解決したが、実際の根本的な難しい問題を回避していないか？

- **4-2. "Happy Path" Bias**: (Pass/Fail)
  エラーハンドリング、エッジケース、潜在的な失敗シナリオを無視していないか？

- **4-3. Over-Engineering**: (Pass/Fail)
  不必要に複雑なソリューションを提案していないか？

- **4-4. Factual Accuracy & Hallucination**: (Pass/Fail)
  すべての技術的詳細が検証可能な正確さか？

---

### 5. Risk & Mitigation Analysis

- **5-1. Overlooked Risks**:
  提案を実装した場合の実際的なリスク・悪影響トップ3

- **5-2. Alternative Scenarios**:
  検討しなかった根本的に異なるアプローチは何か？

---

### 6. Synthesis & Improved Proposal

- **6-1. Summary of Flaws**:
  発見された最も重大な弱点を箇条書きで要約

- **6-2. Revised Confidence Score**:
  この分析に基づき、元の提案への確信度を1-10で再評価

- **6-3. Improved Version**:
  上記で特定した欠陥に基づき、**実際の改善版**を提供。
  変更を記述するだけでなく、より良い回答を書く。

- **6-4. Actionable Next Step**:
  元のアドバイスに従う**前に**ユーザーが取るべき最も重要なアクション

---

## 成功基準

1. 初期回答の重大な欠点が少なくとも1つ特定されている
2. 改善版が具体的に提示されている（抽象的な改善案ではなく実際の回答）
3. 信頼度スコアが根拠に基づいて再評価されている

## 原則

- 前回の回答を**擁護・正当化しない**
- 積極的に弱点、隠れた仮定、見落とされたリスクを特定する
- 懐疑的、詳細志向、徹底的に正直な分析者として機能する
