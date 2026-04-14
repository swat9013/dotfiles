# Harness Engineering ドメインモデル

ドメインモデリング（8ループ）で得られた知見の統合。ハーネスの構造・制御メカニズム・自己メンテナンスの全体像を定義する。

---

## 1. 目標構造

ハーネスの存在意義は**価値観の遵守**——少ない指示で、ユーザーの設計・実装の価値観に沿った行動をエージェントが自律的に取れること。

```
価値観の遵守
├── 前提: 再現性（別セッションでの同等品質）
└── 前提: 精度（ハレーション・逸脱の少なさ）
```

---

## 2. ハーネスの構成

### 2.1 コンテキスト

エージェントに渡す情報の総体。rule, skill, CLAUDE.md で構成される。

| 構成要素 | 性質 | 役割 |
|---------|------|------|
| rule | 推論的 | エージェントの行動を方向づける。LLMが文意を解釈して反映 |
| skill | 推論的 | 特定タスクに対する専門化された手順・知識の単位 |
| CLAUDE.md | 推論的 | 全体方針・常時適用の指示 |

**設計原則（Guide First等）はコンテキスト構成要素ではない。** rule/skillの設計判断基準であり、メタレベルの概念。

### 2.2 制御メカニズム

#### Feedforward（事前方向づけ）

すべて推論的。rule と skill がLLMの行動を事前に誘導する。

#### Feedback（事後検出）

| 手段 | 性質 | 実装 |
|------|------|------|
| hook → lint/test | 計算的 | PreToolUse/PostToolUse フック |
| permissions.deny | 計算的 | settings.json の拒否リスト |
| opusレビュー | 推論的 | サブエージェント委譲による品質検証 |
| スキル内LLM評価 | 推論的 | planの完了定義→LLM判定等 |

#### 計算的制御の4分類

| カテゴリ | 説明 | 現状 |
|---------|------|------|
| A: ガード | 禁止操作の遮断 | 充実（permissions.deny, guard-*.sh） |
| B: 検証 | 品質チェック | 部分的（ruff-check.sh, Pythonのみ） |
| C: 注入 | 情報制御 | 部分的（paths条件付き注入, skill-activation.sh） |
| D: 計測 | 可観測性 | 部分的（PermissionRequest hook + scan-metrics.py） |

**改善の優先順位: D → C。** 何が起きているか見えれば（D）、何を渡すべきか（C）の判断材料になる。

#### 推論的→計算的の昇格基準（B 検証の充実トリガー）

Feedforward（推論的）で書かれている規則・手順のうち、以下を満たすものは Feedback（計算的）に昇格させる。B(検証) が「部分的」から充実へ進む意思決定軸。

| 判定軸 | 昇格対象 | 残す対象 |
|-------|---------|---------|
| 決定論性 | 数値カウント・正規表現マッチ・構造チェック・ファイル存在確認・命名規約・重複検出 | 意味解釈・品質評価・設計判断・因果推定 |
| 違反/実行頻度 | 繰り返し同じ違反が発生、または毎セッション同処理をLLMが反復 | 稀にしか発生しない / 都度判断が要る |
| 効果 | トークン削減・context lot 削減・再現性向上のいずれかが見込める | 効果が限定的で維持コストが上回る |

昇格先は対象により異なる:

| 昇格元 | 昇格先 | 実装形態 |
|-------|-------|---------|
| skill 内の LLM 手順（決定論的処理） | skill の `scripts/` | Python/shell script、SKILL.md から呼び出し |
| rules の決定論的規則 | hook scripts | PreToolUse/PostToolUse/Stop/SessionEnd hook + settings.json 登録 |

昇格運用の責務は harness-tuning（検出・診断）と /update-config（hook 実装）に分離。harness-tuning の診断カテゴリ Cat-9（skill内）/ Cat-10（rules）が本基準の運用窓口。

---

## 3. コンテキスト最適化

### 3.1 トレードオフ

```
価値観の遵守 ──要求する──→ コンテキスト充実
コンテキスト充実 ──増加させる──→ context lot（コンテキスト消費量）
context lot ──引き起こす──→ lost in the middle
lost in the middle ──引き起こす──→ ハレーション
```

### 3.2 粒度ジレンマ

具体的すぎる → context lot増大 + 柔軟性喪失。抽象的すぎる → LLMが解釈を外す。

緩和手段: memory → 抽象化サイクルで段階的に適正粒度を発見する。

### 3.3 Progressive Disclosure（4手段）

| 手段 | メカニズム |
|------|-----------|
| paths条件付き注入 | ファイルパスに基づいてruleを選択的に注入 |
| descriptionトリガー | スキルのdescriptionマッチで遅延読み込み |
| フェーズ分離 | research→plan→implement→reviewで各ステップのコンテキストを限定 |
| サブエージェント委譲 | 別コンテキストウィンドウで処理（レビューバイアス排除効果あり） |

---

## 4. 3領域とSteering Loop

### 4.1 3領域

| 領域 | 定義 |
|------|------|
| コード生成 | ソースコード・設定ファイル・ドキュメントの生成・変更 |
| 環境操作 | シェルコマンド・ツール実行を通じた外部状態の変化 |
| 自己管理 | ハーネス自身の診断・更新・最適化 |

領域をまたぐ処理は明示的なインターフェース（判定キー・ファイル契約）を経由する。

### 4.2 Steering Loop

自己メンテナンス原則の運用サイクル: **劣化検出 → 診断 → 修正 → 検証**

```mermaid
flowchart LR
    D[劣化検出] --> N[診断]
    N --> F[修正]
    F --> V[検証]
    V --> D
```

```
知見源              捕捉(自動化)      蓄積          昇格(人間レビュー)

retrospective ───→ memory ─────┐
逸脱FB ──────────→ memory ────┤──→ claude-config ──→ rule/skill/CLAUDE.md
opusレビュー ────→ tmp/review ─┘    (昇華+頻出抽出)    (価値観一致確認)
```

#### 責務分離の原則

- **retrospective**: 知見の「捕捉」に専念（memory蓄積のみ）
- **claude-config**: 「昇格」に専念（memory/tmp → rule/skill/CLAUDE.md）

retrospective が捕捉と昇格の両方を担うと、昇格の質が落ちる。診断・最適化の専門性が不足するため。

#### 3つの知見源

| 知見源 | 現状 |
|--------|------|
| retrospective | スキル存在、手動実行のみ（→§6.2） |
| セッション中の逸脱FB | Auto Memory + retrospective事後回収で補完（→§6.3） |
| opusレビュー結果 | retrospective が tmp/review → memory に捕捉、claude-config で昇格判定 |

#### 計測データの流れ

| データ | 収集 | 蓄積先 | 消費 |
|--------|------|--------|------|
| PermissionRequest | hook（log-permission-request.sh） | `~/.claude/tmp/metrics/permission-requests.jsonl` | scan-metrics.py → claude-config Agent 1 |
| セッション定量 | セッションJSONLを直接走査 | `~/.claude/projects/*/*.jsonl` | scan-metrics.py → claude-config Agent 1 |
| opusレビュー結果 | implement / claude-config のレビューサイクル | `~/.claude/tmp/review/*.md` | retrospective → memory → claude-config で昇格判定 |

#### retrospective パイプライン分離（Computational First 原則）

retrospective は2パイプラインに分離済み。混在させないこと。

| パイプライン | スクリプト | 入力 | 出力 |
|------------|----------|------|------|
| **定性** | `retrospective/scripts/collect.py` | セッション会話 + `tmp/review/*.md` | 定性データ JSON |
| **定量** | `claude-config/scripts/scan-metrics.py` | `permission-requests.jsonl` | tool別カウント JSON |

- collect.py は定性データ収集のみ。metrics 集計ロジックを追加しない
- scan-metrics.py が全数値集計を担当

---

## 5. 計測（Observability）戦略

### 5.1 計測対象

| # | 対象 | 指標例 |
|---|------|--------|
| ① | ツール呼び出しパターン | Bash拒否率, 同一ファイル繰り返しRead/Edit, guardブロック回数, PermissionRequest回数 |
| ② | コンテキスト消費 | 注入rules/skills本数・総行数, サブエージェント起動数 |
| ③ | 品質指標 | PostToolUse lint失敗率, ユーザーのツール拒否回数（逸脱の代理指標） |

### 5.2 2段構えの実装

| Tier | 手段 | 取得可能な指標 |
|------|------|---------------|
| Tier 1 | retrospective経由（JSONL解析） | tool_use回数, 同一ファイル繰り返し, guardブロック(is_error), Agent起動数, lint失敗(is_error), turn_duration（定性データ・会話JSONLのみ） |
| Tier 2 | hook追加 → scan-metrics.py（claude-config Agent 1） | PermissionRequest回数, ユーザーツール拒否回数 |

---

## 6. 課題と解決方針

### 6.1 計測データの消費フロー（Steering Loopの設計不足）

**問題**: データは取れるが「誰がいつ読むか」が未定義。

**方針**: §4.2の2段構え（retrospective=捕捉、claude-config=昇格）で運用中。トリガーの自動化が未解決（→§6.2）。

### 6.2 retrospective定期トリガー（知見源の安定化）

**問題**: retrospective は手動実行のみ。定期実行の仕組みがない。

**方針**: CronCreate（Claude Code組み込み）を検証し、不安定ならlaunchd + `claude -p` にフォールバック。

- CronCreate で `/retrospective --since=1d` を日次登録
- SessionEnd hookは重い処理に不向き（現行も軽量処理のみ）、UserPromptSubmit hookはユーザー体験を損なうため不採用
- フォールバック: macOS launchd で `claude -p` を定期実行

### 6.3 逸脱FBの属人性軽減（§6.2に依存）

**問題**: セッション中の逸脱FBがユーザーの集中力に依存し、漏れが多い。

**方針**: Auto Memory（ベースライン）+ retrospective事後回収（補完層）の併用。

- Auto Memory の feedback型memory が明確な修正指示を捕捉（既に動作中）
- retrospective が JSONL からAuto Memoryが拾いきれなかったパターンを事後検出（暗黙の不満、ツール拒否、同一修正の繰り返し）
- hookからLLM推論を呼ぶリアルタイム検出は、コスト・レイテンシ的に非現実的なため不採用

---

## 7. 関連図

| 図 | パス | 内容 |
|----|------|------|
| Steering Loop詳細 | `docs/steering-loop.drawio` | 3知見源→蓄積→claude-config→昇格の流れ。欠損エッジを破線で表現 |
| ハーネス全体構造 v2 | `docs/harness-engineering-full-v2.drawio` | 3層構造（目標→ハーネス→Steering Loop）+ メタレベル設計原則 + Observability層 |
