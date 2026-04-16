# Harness Engineering ドメインモデル

ドメインモデリング（前8ループ + 2026-04-15〜16 の6ループ）で得られた知見の統合。ハーネスの**ドメイン階層**・制御構造・自己メンテナンスの全体像を定義する。

参考: https://martinfowler.com/articles/harness-engineering.html — harness engineering は context engineering の部分集合として位置付けられる。

---

## 1. 目標構造

ハーネスの存在意義は**価値観の遵守**——少ない指示で、ユーザーの設計・実装の価値観に沿った行動をエージェントが自律的に取れること。

```
価値観の遵守
├── 前提: 再現性（別セッションでの同等品質）
└── 前提: 精度（ハレーション・逸脱の少なさ）
```

---

## 2. ドメイン階層

ハーネスは3つの規制ドメイン（Regulation Categories）で構成される。目的関数はコンポーネント単位ではなく**ドメイン単位**に置く。

| 語 | 含意 |
|------|------|
| **ドメイン** | 責務軸。目的関数を持つ。「何を達成するか」 |
| **コンポーネント** | 実装軸。ドメインの実装手段。「何で実装するか」 |

1コンポーネントが複数ドメインに貢献することを許容する（コンポーネントは実装であり、実装は複数の責務を同時に担う）。

この3ドメイン構造（§2.1 Maintainability Harness / §2.2 Architecture Fitness Harness / §2.3 Behaviour Harness）は OKR-SLO-Guardrail モデルと準同型である——OKR（達成目標）= Maintainability、SLO（健全性指標）= Architecture Fitness、Guardrail（逸脱防止）= Behaviour という対応関係を持つ。

### 2.1 Maintainability Harness
<a id="maintainability-harness"></a>

| 要素 | 内容 |
|------|------|
| 対象 | 生成コード・リポジトリの保守性 |
| 最大化価値 | 生成コードが長期保守可能な状態を維持する |
| 達成シグナル | 品質ゲートPASS率が高位安定 / 型エラー・lint違反が低位安定 / リファクタ容易性が損なわれない |
| 境界条件 | 過剰な自動修正で意図を歪めない / 偽陽性で開発を止めない |

### 2.2 Architecture Fitness Harness
<a id="architecture-fitness-harness"></a>

| 要素 | 内容 |
|------|------|
| 対象 | harness自体の健全性（自己メンテナンス） |
| 最大化価値 | harness自身の劣化を検出・修正し続ける |
| 達成シグナル | Harness劣化シグナルがSteering Loopで回収される / 未解決の逸脱が蓄積しない / ドメインモデルと実装の乖離が小さい |
| 境界条件 | 自己メンテナンスが本業を侵食しない / 計測のための計測にならない |

### 2.3 Behaviour Harness
<a id="behaviour-harness"></a>

| 要素 | 内容 |
|------|------|
| 対象 | エージェント実行時の行動（価値観遵守） |
| 最大化価値 | ユーザー価値観に沿った自律行動を少ない指示で実現 |
| 達成シグナル | ユーザー修正指示の反復がない / Feedforwardで自己抑制が効く / Feedbackで逸脱が検出される |
| 境界条件 | 過剰規則化で表現を硬直化させない / 常時必要な指示をrulesに退避しない |

### 2.4 三部構造（Goodhart対策）

各ドメインの目的関数は以下の三部で記述する。単一数値の目的関数は劣化を加速するため採用しない。

- **最大化価値**: 定性1文。「このドメインがなければ何がどう壊れるか」から逆算
- **達成シグナル**: 価値達成の観測可能な兆候。複数並列必須
- **境界条件**: 追求時に犠牲にしてはならない制約

### 2.5 独立レビュー（旧opusレビュー）

責務命名で `opusレビュー` → `独立レビュー`（責務: 生成バイアス排除）。メカニズム/インスタンス階層で扱う:

- **メカニズム**: 独立レビュー（定義）
- **インスタンス**: 各スキルのレビューフェーズ / claude-config診断の独立実行
- **再帰評価**: 独立レビュー自体を harness-tuning の独立レビューで評価

独立レビューのプロンプトには**ドメイン目的関数の三項組**（最大化価値 + 達成シグナル + 境界条件）を注入する必要がある。汎用品質基準ではなくドメイン参照点を与えることで、些末な指摘への退行を防ぐ（現状は注入済み（2026-04-16）、詳細は §9.4）。

#### 参照指示パターン

独立レビュープロンプトに判断基準となる参照情報（ドメイン目的関数の三項組）を明示的に注入するパターン。汎用品質基準ではなく **ドメイン固有の参照点**（最大化価値・達成シグナル・境界条件）を渡すことで、レビュアーの判断をドメイン目的関数に沿わせる。実装上は `_shared/independent-review-prompt.md` を集約ハブとして各スキルから参照注入する（ハブ経由注入）。Read指示パターンからの改称（改称理由: 「Read」はツール名由来で責務が不明確、「参照指示」は「判断基準を参照させる」責務を直接示す）。

#### ハブ経由注入

`_shared/independent-review-prompt.md` を集約ハブとして機能させ、各スキルがそこを参照することでドメイン目的関数の三項組（最大化価値・達成シグナル・境界条件）を受け取る構造。各スキルの独立レビューフェーズはハブを経由して目的関数を注入されるため、スキルごとに目的関数を個別記述する必要がない。独立レビュー目的関数の唯一の正本を集約ハブに置くことで、記述分散と更新漏れを防ぐ（観測粒度軸の再帰適用: 独立レビューという観測単位ごとに配置先を統一する）。

---

## 3. 二軸制御構造

制御は2つの軸で分類される（どちらもドメイン横断で適用）。

| 軸 | 値 | 説明 |
|---|---|---|
| 実行タイプ | **Computational** | 決定論的。スクリプト・設定で実行 |
| | **Inferential** | 推論的。LLMが文意を解釈 |
| 時系列軸 | **Feedforward (Guides)** | 事前方向づけ |
| | **Feedback (Sensors)** | 事後検出 |

### 3.1 4象限とコンポーネント配置

| | Feedforward | Feedback |
|---|---|---|
| **Inferential** | CLAUDE.md / rule / skill（段階開示） | 独立レビュー / claude-config診断 |
| **Computational** | skill内のスクリプト注入 / 事前品質ゲート | settings.json / hook / 事後品質ゲート |

### 3.2 設計原則

- **Guide First, Sense Second**: Feedforward優先。予防的誘導を事後検出より先行させる
- **Computational First**: 計測可能な基準はスクリプト化する。推論に委ねるのは判断が必要なものだけ

### 3.3 計算的制御の4分類

| カテゴリ | 説明 | 現状 |
|---------|------|------|
| A: ガード | 禁止操作の遮断 | 充実（permissions.deny, guard-*.sh） |
| B: 検証 | 品質チェック | 部分的（ruff-check.sh, Pythonのみ） |
| C: 注入 | 情報制御 | 部分的（paths条件付き注入, skill-activation.sh） |
| D: 計測 | 可観測性 | 部分的（PermissionRequest hook + scan-metrics.py） |

**改善の優先順位: D → C。** 何が起きているか見えれば（D）、何を渡すべきか（C）の判断材料になる。

### 3.4 推論的→計算的の昇格基準

Feedforward（推論的）で書かれている規則・手順のうち、以下を満たすものは Feedback（計算的）に昇格させる。

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

昇格運用の責務は harness-tuning（検出・診断）と /update-config（hook 実装）に分離。

---

## 4. コンポーネント層

### 4.1 コンポーネント5種（観測粒度軸）

Harnessから独立に観測される粒度でコンポーネントを定義する。

| コンポーネント | 主制御層 | 役割 |
|---|---|---|
| **skill** | Inferential Feedforward（+内部でComputational併用） | 特定ワークフロー実行 + path非依存の専門知識段階開示 |
| **rule** | Inferential Feedforward | path依存の段階開示・行動方向づけ |
| **hook** | Computational（pre=Feedforward / post=Feedback） | 実行時介入。複数層にまたがり得る |
| **CLAUDE.md** | Inferential Feedforward | グローバル行動指針の常時注入 |
| **settings.json** | Computational Feedback | 禁止操作管理・権限制御 |

**非コンポーネント**: SKILL.md / references/ は skill の内部詳細。Harnessからは skill 単位で観測するため、これら内部構造は独立コンポーネントとして扱わない。

### 4.2 ドメイン-コンポーネント対応

1コンポーネントが複数ドメインに貢献することを許容する。

| ドメイン | 主実装 | 補助実装 |
|---|---|---|
| Maintainability Harness | coding-principles rule / ruff / pyright / pytest / 品質ゲート | CLAUDE.mdのコーディング指針部 |
| Architecture Fitness Harness | harness-architecture rule / retrospective / claude-config / 独立レビュー / Steering Loop | コナセンス管理 |
| Behaviour Harness | CLAUDE.md / dev-cycle rule / 各workflow skill / settings.json / hook / memory | 品質ゲート（Behaviour文脈でのFeedback） |

### 4.3 skill の目的関数記述（選択肢A: skill独立型）

各skillは独自の三部構造を冒頭 frontmatter 等に持つ。親ドメイン（Behaviour Harness等）への階層明示はしない。

**理由**: Behaviour Harnessドメインの達成シグナル（ユーザー修正指示の反復がない／Feedforwardで自己抑制が効く）は**skillより上の層で観測される現象**で、個別skill達成の集約ではない。階層明示は不要。

---

## 5. サブエージェント設計

### 5.1 フラクタル構造

サブエージェント実行環境はHarnessの自己相似。サブエージェントは**コンポーネントの一種**として統合する。「サブエージェント目的関数」を独立概念として立てず、ドメイン目的関数の三部構造をそのまま適用する。

ただし実装形態は階層ごとに調整する:

- **静的サブエージェント**: 定義ファイル内に自己記述
- **動的サブエージェント**: 定義ファイルが物理的に存在しないため、**親スキル側**に目的関数を書く

### 5.2 委譲価値6類型

| # | 価値 | 方向 | 機構 |
|---|------|-----------|------|
| 1 | コンテキスト分離 | 親を守る（汚染防止） | コンテキスト境界 |
| 2 | バイアス排除 | 子を守る（影響防止） | コンテキスト境界 |
| 3 | 並列化 | 壁時計時間短縮 | 同時実行 |
| 4 | 専門化 | ドメイン特化判断 | prompt/role注入 |
| 5 | 使い捨て実行 | 失敗耐性 | single-shot |
| 6 | コスト最適化 | 推論深度・料金・レイテンシ調整 | モデル階層使い分け |

1と2は機構同一（コンテキスト境界）だが目的の向きが逆のため別価値として扱う。重ね合わせは許容。

### 5.3 親子責務分担の4基準

| # | 基準 | 親に保持 | 子に委譲 |
|---|------|---------|---------|
| 1 | 不可逆性 | 親で可能なもの | 親では構造的に不可能（例: バイアス排除） |
| 2 | コンテキスト経済性 | 結論・意思決定 | 広範な探索・調査 |
| 3 | 意思決定権（普遍ルール） | **最終判断・ユーザー対話** | 材料収集・検証 |
| 4 | 再現性要求 | 柔軟判断が必要なもの | 再現性重視（狭いコンテキスト + 明示prompt） |

#### 親子目的関数非対称配置

親スキルと子サブエージェントで**主目的関数のドメインが異なる**配置設計。例: claude-config 本体は Architecture Fitness 主（harness自体の健全性）だが、Agent 1/2 は Behaviour Harness 主（settings.json / CLAUDE.md の行動指針診断）、Agent 3 は Maintainability Harness 主（skills の保守性診断）となる。非対称配置は設計上の欠陥ではなく、**診断対象ドメインに一致させる**意図的な選択（診断対象ドメイン一致原則を参照）。設計意図が SKILL.md に未記述だと階層不整合リスクになるため、判別可能性（スキル設計意図の自己記述原則）で明示が必要。階層的目的関数切り替えからの改称（改称理由: 「切り替え」は動的操作の連想を与えるが実態は設計時の静的配置決定）。

### 5.4 静的/動的軸

| 分類 | 特徴 | 目的関数の所在 | 例 |
|---|---|---|---|
| 静的 | 定義ファイルあり、prompt事前確定 | 子の定義ファイル | Explore / Plan / 独立レビュー / claude-config診断 / researcher / code-reviewer / claude-code-guide |
| 動的 | 親が実行時にprompt生成 | **親スキル側** | architect内部の動的呼び出し等 |

#### 診断対象ドメイン一致原則

各サブエージェント（特にclaude-config Agent群）の**主目的関数は診断対象ドメインに一致させる**原則。Agent 1/2 は settings.json / CLAUDE.md を診断するため `#behaviour-harness` 主、Agent 3 は skills を診断するため `#maintainability-harness` 主となる。本体（Architecture Fitness主）との切り替えは設計判断であり、親子目的関数非対称配置の一形態。この原則により非対称配置が正当化される。診断対象適合からの改称（改称理由: 「適合」は条件充足の受動的ニュアンス、「一致原則」は設計者が能動的に合わせることを要求する規範性を明示）。

### 5.5 静的定義昇格原則

反復利用されるサブエージェントは静的定義に昇格させる。

**根拠**:

- コンテキスト経済性（親がprompt組み立てコンテキストを消費しない）
- 再現性向上
- 目的関数を子側に置ける（フラクタル構造の実装上の要件を満たす）
- 独立レビュー対象になり得る

---

## 6. コンテキスト最適化

### 6.1 トレードオフ

```
価値観の遵守 ──要求する──→ コンテキスト充実
コンテキスト充実 ──増加させる──→ context lot（コンテキスト消費量）
context lot ──引き起こす──→ lost in the middle
lost in the middle ──引き起こす──→ ハレーション
```

### 6.2 粒度ジレンマ

具体的すぎる → context lot増大 + 柔軟性喪失。抽象的すぎる → LLMが解釈を外す。

緩和手段: memory → 抽象化サイクルで段階的に適正粒度を発見する。

### 6.3 Progressive Disclosure（4手段）

| 手段 | メカニズム |
|------|-----------|
| paths条件付き注入 | ファイルパスに基づいてruleを選択的に注入 |
| descriptionトリガー | スキルのdescriptionマッチで遅延読み込み |
| フェーズ分離 | research→plan→implement→reviewで各ステップのコンテキストを限定 |
| サブエージェント委譲 | 別コンテキストウィンドウで処理（レビューバイアス排除効果あり） |

---

## 7. 3領域とSteering Loop

### 7.1 3領域

| 領域 | 定義 |
|------|------|
| コード生成 | ソースコード・設定ファイル・ドキュメントの生成・変更 |
| 環境操作 | シェルコマンド・ツール実行を通じた外部状態の変化 |
| 自己管理 | ハーネス自身の診断・更新・最適化 |

領域をまたぐ処理は明示的なインターフェース（判定キー・ファイル契約）を経由する。

### 7.2 Steering Loop

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
独立レビュー ─────→ tmp/review ─┘    (昇華+頻出抽出)    (価値観一致確認)
```

#### 責務分担の原則

- **retrospective**: 知見の「捕捉」に専念（memory蓄積のみ）
- **claude-config**: 「昇格」に専念（memory/tmp → rule/skill/CLAUDE.md）

retrospective が捕捉と昇格の両方を担うと、昇格の質が落ちる。診断・最適化の専門性が不足するため。

#### 3つの知見源

| 知見源 | 現状 |
|--------|------|
| retrospective | スキル存在、手動実行のみ（→§9.2） |
| セッション中の逸脱FB | Auto Memory + retrospective事後回収で補完（→§9.3） |
| 独立レビュー結果 | retrospective が tmp/review → memory に捕捉、claude-config で昇格判定 |

#### 計測データの流れ

| データ | 収集 | 蓄積先 | 消費 |
|--------|------|--------|------|
| PermissionRequest | hook（log-permission-request.sh） | `~/.claude/tmp/metrics/permission-requests.jsonl` | scan-metrics.py → claude-config Agent 1 |
| セッション定量 | セッションJSONLを直接走査 | `~/.claude/projects/*/*.jsonl` | scan-metrics.py → claude-config Agent 1 |
| 独立レビュー結果 | implement / claude-config のレビューサイクル | `~/.claude/tmp/review/*.md` | retrospective → memory → claude-config で昇格判定 |

#### retrospective パイプライン分離（Computational First 原則）

retrospective は2パイプラインに分離済み。混在させないこと。

| パイプライン | スクリプト | 入力 | 出力 |
|------------|----------|------|------|
| **定性** | `retrospective/scripts/collect.py` | セッション会話 + `tmp/review/*.md` | 定性データ JSON |
| **定量** | `claude-config/scripts/scan-metrics.py` | `permission-requests.jsonl` | tool別カウント JSON |

- collect.py は定性データ収集のみ。metrics 集計ロジックを追加しない
- scan-metrics.py が全数値集計を担当

---

## 8. 計測（Observability）戦略

### 8.1 計測対象

| # | 対象 | 指標例 |
|---|------|--------|
| ① | ツール呼び出しパターン | Bash拒否率, 同一ファイル繰り返しRead/Edit, guardブロック回数, PermissionRequest回数 |
| ② | コンテキスト消費 | 注入rules/skills本数・総行数, サブエージェント起動数 |
| ③ | 品質指標 | PostToolUse lint失敗率, ユーザーのツール拒否回数（逸脱の代理指標） |

### 8.2 2段構えの実装

| Tier | 手段 | 取得可能な指標 |
|------|------|---------------|
| Tier 1 | retrospective経由（JSONL解析） | tool_use回数, 同一ファイル繰り返し, guardブロック(is_error), Agent起動数, lint失敗(is_error), turn_duration（定性データ・会話JSONLのみ） |
| Tier 2 | hook追加 → scan-metrics.py（claude-config Agent 1） | PermissionRequest回数, ユーザーツール拒否回数 |

---

## 9. 課題と解決方針

### 9.1 計測データの消費フロー（Steering Loopの設計不足）

**問題**: データは取れるが「誰がいつ読むか」が未定義。

**方針**: §7.2の2段構え（retrospective=捕捉、claude-config=昇格）で運用中。トリガーの自動化が未解決（→§9.2）。

### 9.2 retrospective定期トリガー（知見源の安定化）

**問題**: retrospective は手動実行のみ。定期実行の仕組みがない。

**方針**: CronCreate（Claude Code組み込み）を検証し、不安定ならlaunchd + `claude -p` にフォールバック。

- CronCreate で `/retrospective --since=1d` を日次登録
- SessionEnd hookは重い処理に不向き（現行も軽量処理のみ）、UserPromptSubmit hookはユーザー体験を損なうため不採用
- フォールバック: macOS launchd で `claude -p` を定期実行

### 9.3 逸脱FBの属人性軽減（§9.2に依存）

**問題**: セッション中の逸脱FBがユーザーの集中力に依存し、漏れが多い。

**方針**: Auto Memory（ベースライン）+ retrospective事後回収（補完層）の併用。

- Auto Memory の feedback型memory が明確な修正指示を捕捉（既に動作中）
- retrospective が JSONL からAuto Memoryが拾いきれなかったパターンを事後検出（暗黙の不満、ツール拒否、同一修正の繰り返し）
- hookからLLM推論を呼ぶリアルタイム検出は、コスト・レイテンシ的に非現実的なため不採用

### 9.4 ドメイン目的関数の実体化

**【解決済み: 2026-04-16】**

注入は 2026-04-16 の Phase 1〜3 で完了。注入方式・対象の詳細は §12 モデリング履歴を参照。残存論点は §9.5 を参照。

### 9.5 持ち越しの未解決論点

- サブエージェント二面性の問題（重複表記 / 階層不整合 / 記述分散）: 試作時に観察して判断
- 判断基準reference と ドメイン最大化価値の近接（混同リスク）
- タスク完了定義 と サブエージェント成功基準 の境界
- memory昇格判定 でのドメイン目的関数の使われ方

---

## 10. 命名規則（語彙のメタルール）

### 10.1 責務命名優先

採用技術命名ではなく責務命名を選ぶ。

- 採用技術命名（例: `opusレビュー` ← モデル名由来）はモデル変更で破綻する
- 責務命名（例: `独立レビュー` ← 「生成バイアス排除」の責務）は技術変更耐性が高い

### 10.2 スコープ依存語は一律接頭辞化（例外なし）

LLMは複数ファイルを同時にコンテキストに入れ、ファイル境界は保存されない。「このファイル内では○○の文脈」という暗黙知はLLM側で復元不能。

**スコープ軸**: `Harness` > `コンポーネント` > `Phase` > `タスク` > `サブエージェント`

**接頭辞化例**:

- `成功基準` → `サブエージェント成功基準`
- `受け入れ条件` → `Phase受け入れ条件`
- `完了定義` → `タスク完了定義`
- `判定キー` → `スクリプト判定キー`
- `達成シグナル` → `コンポーネント達成シグナル` / `Harness劣化シグナル`（対称性を命名に反映）

**対象外**: 固有名詞（`Steering Loop`, `3領域`）と概念分類語（`Feedforward`, `Computational`）は類似語が存在しないため接頭辞不要。

---

## 11. 関連図

| 図 | パス | 内容 |
|----|------|------|
| Harness 語彙モデル | `docs/harness-vocabulary-model.drawio` / `.png` | ドメイン階層 / 3ドメイン三部構造 / 二軸制御構造 / ドメイン横断原則 / コンポーネント-ドメイン対応 |
| Steering Loop詳細 | `docs/steering-loop.drawio` | 3知見源→蓄積→claude-config→昇格の流れ。欠損エッジを破線で表現 |
| ハーネス全体構造 v2 | `docs/harness-engineering-full-v2.drawio` / `.png` | 3層構造（目標→ハーネス→Steering Loop）+ メタレベル設計原則 + Observability層。ドメイン名・独立レビュー命名を6ループ語彙に更新済み |

---

## 12. モデリング履歴

| ラウンド | 日付 | 主要な合意 |
|---|---|---|
| 前8ループ | 〜2026-04-14 | 制御メカニズム / Steering Loop / 計測戦略 / Progressive Disclosure / 3領域 |
| 本6ループ | 2026-04-15 〜 2026-04-16 | ドメイン階層の導入 / 目的関数の所在をコンポーネント→ドメインに昇格 / 二軸制御構造 / コンポーネント5種確定 / サブエージェント設計原則（フラクタル構造・委譲価値6類型・責務分担4基準・静的/動的軸・静的定義昇格原則）/ 独立レビュー命名への変更 |

- 2026-04-16: ドメイン目的関数の実行経路への注入完了（claude-config Agent 1/2/3、R2独立レビュー、implement/review-fix/architect/plan/breakdown/harness-tuning の独立レビュー、命名「独立レビュー」統一）

詳細ログ: `docs/harness-vocabulary-modeling-history.md`
