---
name: domain-modeling
description: |-
  ドメインモデリングの手法選択・記述法・設計原則のガイド。
  手法の段階的組み合わせ、記述法の目的別選択、アンチパターン回避を支援する。
  Use when「ドメインモデリング」「DDD」「Bounded Context」「Event Storming」「ドメイン設計」「集約」「Aggregate」「ドメインイベント」。
user-invocable: false
---

# Domain Modeling ガイド

## 手法選択マトリクス

プロジェクト段階に応じて手法を組み合わせる。単一手法の採用ではなく段階的組み合わせが基本。

| 段階 | 手法 | 目的 | 所要時間 |
|------|------|------|----------|
| 全体把握 | Event Storming (Big Picture) | ドメイン境界・主要イベントの発見 | 数時間〜1日 |
| 知識深掘り | Domain Storytelling | エキスパートの詳細知識抽出、Ubiquitous Language 構築 | 複数セッション |
| 戦略的設計 | DDD (Bounded Context / Context Map) | ドメイン分割、チーム境界の設定 | 継続的 |
| タクティカル設計 | Event Storming (Process/Design) / Event Modeling | コマンド・イベント・集約の具体化 | 数時間〜数日 |
| 検証・実装 | Example Mapping (BDD) | 受け入れ基準の明確化、自動テスト化 | 25分/回 |

各手法の詳細 → `references/methodologies.md`

## 記述法の選択

| 目的 | 推奨記述法 | 理由 |
|------|----------|------|
| 概念モデル（Entity/VO/Aggregate） | UML クラス図 | 構造と関係の厳密な表現 |
| システム全体像の共有 | C4 モデル (Context/Container) | 技術者・非技術者の共通理解 |
| Bounded Context 間関係 | Context Map | 9つの関係パターン（ACL, Shared Kernel等） |
| ドキュメント内の図 | Mermaid | Git管理可能、GitHub直接レンダリング |
| 厳密な仕様書 | PlantUML | UML準拠、規制対応 |
| 自由形式の図 | draw.io | GUI直感的、多様な図種 |
| 設計判断の記録 | ADR | 判断理由と代替案の永続化 |

各記述法の詳細 → `references/notations.md`

## モデリング原則

1. **Ubiquitous Language** — 同じ概念に同じ用語。コード・テスト・ドキュメント全体に反映
2. **Bounded Context** — 意味が一貫する領域ごとに独立したモデルを持つ
3. **Rich Domain Model** — ビジネスロジックは Entity/Value Object に配置。Service層は薄く
4. **小さな Aggregate** — 他 Aggregate への参照は ID のみ。トランザクション境界を明確に
5. **テスト = 仕様** — ドメイン言語でビジネスルールを検証可能な形で表現
6. **継続的精緻化** — モデルは「生きたドキュメント」。開発中の気づきを即反映
7. **永続化の分離** — Repository層で隔離。Domain Logic に技術的詳細を混入させない

## アンチパターン

| パターン | 症状 | 対策 |
|---------|------|------|
| Anemic Domain Model | Entity が getter/setter の袋。ロジックが Service に集中 | ビジネスロジックを Entity/VO に組み込む |
| Big Ball of Mud | 境界なし巨大コード、変更影響が予測不能 | Bounded Context を定義し境界を構築 |
| 過度な抽象化 | シンプルな問題に多数のパターン適用 | KISS。今必要なものだけ実装 |
| Oversized Aggregate | 巨大な集約、パフォーマンス低下 | 小さく保ち、参照は ID のみ |
| Leaky Model | 永続化が Domain Logic に混在 | Repository で完全隔離 |

## チーム運用の要点

- **ドメインエキスパートとの定期協働**: 週1回以上のアクセスを確保
- **Event Storming の実施**: ファシリテーター確保が成否を決める。参加者はドメインエキスパート（必須）+ エンジニア + PdM
- **段階的導入（Strangler Fig）**: レガシーシステムの継ぎ目を特定 → 小規模コンポーネントから段階的に交換

## Context Map 関係パターン

| パターン | 意味 | 適用場面 |
|---------|------|----------|
| Shared Kernel | 共有モデル | 密結合チーム間 |
| Customer/Supplier | 上流/下流の依存関係 | チーム間の非対称依存 |
| Anti-Corruption Layer (ACL) | 変換層で外部モデルを隔離 | レガシー連携、外部API |
| Open Host Service (OHS) | 公開プロトコルでサービス提供 | 複数消費者へのAPI公開 |
| Published Language | 標準化された交換形式 | ドメインイベント共有 |
| Conformist | 上流モデルに従属 | 変換コスト > 適合コスト |
| Separate Ways | 統合しない | 依存関係が不要な場合 |
| Partnership | 協調的な開発 | 対等なチーム間 |
| Big Ball of Mud | 境界不明確 | レガシーシステム（回避対象） |
