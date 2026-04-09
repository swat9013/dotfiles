---
name: security-audit
description: パッケージマネージャのセキュリティ設定を診断し、最新脅威情報に基づく改善を提案する。Use when「セキュリティ監査」「セキュリティチェック」「パッケージセキュリティ」「サプライチェーン」。
argument-hint: "[diagnose|update]"
user-invocable: true
disable-model-invocation: true
---

# Security Audit

## 引数による分岐

- 引数なし or `diagnose` → [Workflow 1: 診断](#workflow-1-診断)
- `update` → [Workflow 2: 更新](#workflow-2-更新)

呼び出し引数: `$ARGUMENTS`

---

## Workflow 1: 診断

`/security-audit` または `/security-audit diagnose` で実行。

### Step 1: audit.sh の実行

Bash tool で以下を実行する。

```
~/.dotfiles/.claude/skills/security-audit/scripts/audit.sh
```

出力末尾の `GATE:` 行で分岐する:
- `GATE: PASS` → Step 4 へ（FAIL なし）
- `GATE: FAIL` → Step 2 へ

`[WARN]` 項目は手動確認が必要な項目として Step 3 で扱う。

### Step 2: FAIL 項目の対策提示

`[FAIL]` が1件以上ある場合、`references/checklist.md` を参照して各 FAIL 項目について以下を提示する。

- 優先度（Critical / High / Medium / Low）
- 具体的な修正手順

提示順は優先度の高い順。

### Step 3: 手動確認項目の調査

`references/checklist.md` で「手動確認」と記載された項目について現環境を調査し、状況を報告する。

対象例:
- サードパーティ tap のリポジトリ信頼性（brew tap の出力が PASS でも要確認）
- ロックファイルのコミット有無（uv.lock / package-lock.json / go.sum 等）

### Step 4: 改善アクションリスト

FAIL 項目 + 手動確認で要対処となった項目を優先度順にリスト化して提示する。

フォーマット例:
```
## 改善アクション（優先度順）

### High
1. UV_REQUIRE_VIRTUALENV を true に設定（~/.zshenv に追記）

### Medium
2. ~/.npmrc に ignore-scripts=true を追加
```

---

## Workflow 2: 更新

`/security-audit update` で実行。

### Step 1: 最新脅威情報の収集

WebSearch で以下のクエリを使用して最新のサプライチェーン攻撃情報を取得する。

推奨クエリ（年は当年を使用）:
- `"supply chain attack" npm pip brew <当年> site:snyk.io OR site:socket.dev OR site:blog.pypi.org`
- `"malicious package" pypi npm <当年>`
- `"typosquatting" package manager attack recent`

`<当年>` は実行時の西暦年に置き換える。

取得した情報から以下を抽出する:
- 新たな攻撃手法・ベクター
- 影響を受けたパッケージマネージャ
- 既知の対策の有効性に関する評価

### Step 2: checklist.md との比較分析

`references/checklist.md` の現在の内容と Step 1 の情報を比較し、以下を分析する。

| 分析項目 | 内容 |
|---------|------|
| 追加すべき項目 | 新たな攻撃手法に対応していない検査 |
| 優先度変更が必要な項目 | 攻撃頻度・被害規模が変化した項目 |
| 廃止すべき項目 | エコシステム側で対応済み・陳腐化した項目 |

### Step 3: 差分提案

以下の形式で変更案を提示する。

```
## checklist.md 更新提案

### 追加（n件）
- [High] 新項目名: 理由・根拠URL

### 優先度変更（n件）
- 既存項目名: High → Medium（理由）

### 廃止（n件）
- 既存項目名: 理由

### audit.sh 更新が必要な項目（n件）
- 項目名: 追加すべきチェックロジックの概要
```

### Step 4: ユーザー承認後の更新

ユーザーが承認した差分のみを適用する。

1. `references/checklist.md` を Edit tool で更新
2. audit.sh に新しいチェックが必要な場合は `scripts/audit.sh` を更新

---

## References

- `references/checklist.md` — 各検査項目の優先度・対策手順・手動確認要否の詳細定義
- `scripts/audit.sh` — 自動検査スクリプト（PASS/FAIL/SKIP を出力）
