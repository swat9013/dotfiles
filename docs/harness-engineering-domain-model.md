# Harness Engineering ドメインモデル

ドメインモデリング（8ループ）で得られた知見の統合。ハーネスの構造・制御メカニズム・自己メンテナンスの全体像を定義する。

> 2026-04-12 ドメインモデリングセッション（8ループ: 語彙採取→モデル化→歪み検証→訂正）の成果を統合。

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
| D: 計測 | 可観測性 | ほぼ不在（statusLineのみ） |

**改善の優先順位: D → C。** 何が起きているか見えれば（D）、何を渡すべきか（C）の判断材料になる。

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

| 知見源 | 現状 | 課題 |
|--------|------|------|
| retrospective | スキル存在、手動実行のみ | 定期トリガー未実装 |
| セッション中の逸脱FB | ユーザーの集中力に依存 | 属人的、漏れが多い |
| opusレビュー結果 | その場で消費して終了 | 頻出パターンの蓄積・還元なし |

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
| Tier 1 | retrospective経由（JSONL解析） | tool_use回数, 同一ファイル繰り返し, guardブロック(is_error), Agent起動数, lint失敗(is_error), turn_duration |
| Tier 2 | hook追加 | PermissionRequest回数, ユーザーツール拒否回数 |

大部分はTier 1（retrospective拡張）で取れる。JSONLにPermissionRequestが記録されないため、不足分のみhook追加が必要。

---

## 6. 課題と解決方針

### 6.1 PermissionRequestの計測（計測の制約）

**問題**: PermissionRequestがJSONLに記録されず、ユーザーの許可/拒否パターンが不可視。

**方針**: `PermissionRequest` hookで外部ログに追記。

- hookスクリプトで `.claude/tmp/metrics/permission-requests.jsonl` に1行1JSON（`timestamp`, `tool`, `input_preview`）を記録
- `PermissionRequest` hookは `stop-open-zed.sh` で確立済みのパターンを踏襲
- retrospective の `collect.py` にこのファイルの読み込みを追加して統合

### 6.2 計測データの消費フロー（Steering Loopの設計不足）

**問題**: データは取れるが「誰がいつ読むか」が未定義。

**方針**: retrospective（捕捉）+ claude-config（昇格）の2段構え。

```
JSONL + hookログ
  → collect.py (retrospective) → memory/metrics_*.md
  → scan-*.py (claude-config)  → 構造改善提案
```

- retrospective: collect.py を拡張し、計測データも収集→分析サブエージェントに渡す。知見として「ガードブロック急増→ruleの粒度問題」等のパターンを検出
- claude-config: retrospective が蓄積した計測memoryを診断の入力に追加し、構造改善に接続

### 6.3 retrospective定期トリガー（知見源の安定化）

**問題**: retrospective は手動実行のみ。定期実行の仕組みがない。

**方針**: CronCreate（Claude Code組み込み）を検証し、不安定ならlaunchd + `claude -p` にフォールバック。

- CronCreate で `/retrospective --since=1d` を日次登録
- SessionEnd hookは重い処理に不向き（現行も軽量処理のみ）、UserPromptSubmit hookはユーザー体験を損なうため不採用
- フォールバック: macOS launchd で `claude -p` を定期実行

### 6.4 opusレビュー頻出抽出（知見フローの欠損）

**問題**: opusレビュー結果が `tmp/review/` に保存されるが、一回きりの消費で終わる。

**方針**: claude-config の診断フェーズに `tmp/review/` 走査を統合。

- §4.2 の責務分離原則に従い、「昇格」の責務を持つclaude-configに配置
- `scan-review.py` を新設（レビューファイルを読み、指摘カテゴリを抽出、頻度カウント）
- 頻出閾値（3回以上）を超えた指摘を昇格候補として診断レポートに含める

### 6.5 逸脱FBの属人性軽減（課題6.3に依存）

**問題**: セッション中の逸脱FBがユーザーの集中力に依存し、漏れが多い。

**方針**: Auto Memory（ベースライン）+ retrospective事後回収（補完層）の併用。

- Auto Memory の feedback型memory が明確な修正指示を捕捉（既に動作中）
- retrospective が JSONL からAuto Memoryが拾いきれなかったパターンを事後検出（暗黙の不満、ツール拒否、同一修正の繰り返し）
- hookからLLM推論を呼ぶリアルタイム検出は、コスト・レイテンシ的に非現実的なため不採用

### 6.6 実装優先順位

| 順位 | 課題 | 依存 | 並列可否 |
|------|------|------|----------|
| 1 | 6.1 PermissionRequest hookログ | なし | ← 1と2は並列着手可 |
| 2 | 6.3 retrospective定期トリガー | なし | ← 1と2は並列着手可 |
| 3 | 6.2 消費フロー設計 | 6.1 | |
| 4 | 6.4 opusレビュー頻出抽出 | 6.2 | |
| 5 | 6.5 逸脱FB補完 | 6.3 | |

方針: D（計測）→ C（注入）の順。計測基盤（6.1, 6.3）を先に整え、消費フロー（6.2）で接続し、知見還元（6.4, 6.5）を上に乗せる。

---

## 7. 関連図

| 図 | パス | 内容 |
|----|------|------|
| Steering Loop詳細 | `docs/steering-loop.drawio` | 3知見源→蓄積→claude-config→昇格の流れ。欠損エッジを破線で表現 |
| ハーネス全体構造 v2 | `docs/harness-engineering-full-v2.drawio` | 3層構造（目標→ハーネス→Steering Loop）+ メタレベル設計原則 + Observability層 |
