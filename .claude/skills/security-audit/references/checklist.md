# セキュリティ検査項目チェックリスト

エコシステム別の検査項目と audit.sh / 手動確認の分担。

## 目次

- [Homebrew](#homebrew)
- [Python (uv / poetry)](#python-uv--poetry)
- [JavaScript (bun / yarn)](#javascript-bun--yarn)
- [Go modules](#go-modules)
- [mise](#mise)
- [エコシステム横断](#エコシステム横断)

---

## Homebrew

| 項目 | 優先度 | 検査方法 | 期待状態 | FAIL時の対策 |
|------|--------|---------|---------|-------------|
| `HOMEBREW_VERIFY_ATTESTATIONS` 環境変数 | High | audit.sh | `true` に設定済み | `.zshrc` に `export HOMEBREW_VERIFY_ATTESTATIONS=true` を追加 |
| サードパーティ tap の一覧確認 | High | audit.sh + 手動確認（audit.sh は一覧表示のみ。要否の判断は手動） | `homebrew/*` 以外のtapが最小限 | 使用していない tap を `brew untap` で削除。残存 tap は用途を記録して棚卸し |
| `brew update && brew upgrade` の定期実行 | Medium | 手動確認 | セキュリティパッチが最新 | `plugin_update.command` のスケジュールを確認・実行 |
| `--no-quarantine` フラグの不使用 | Medium | 手動確認 | `.zshrc` / スクリプトに `--no-quarantine` が含まれない | 該当フラグを削除。Gatekeeperを迂回しないようにする |
| Cask の署名・公証状態 | Low | 手動確認 | インストール済み Cask が署名・公証済み | `spctl --assess --type open --context context:primary-signature` で確認。未署名 Cask は代替を検討 |

---

## Python (uv / poetry)

| 項目 | 優先度 | 検査方法 | 期待状態 | FAIL時の対策 |
|------|--------|---------|---------|-------------|
| `UV_REQUIRE_VIRTUALENV` 環境変数 | Critical | audit.sh | `true` に設定済み | `.zshrc` に `export UV_REQUIRE_VIRTUALENV=true` を追加 |
| `pip.conf` の `require-virtualenv` | Critical | audit.sh | `~/.config/pip/pip.conf` に `require-virtualenv = true` が存在 | `mkdir -p ~/.config/pip && echo -e "[global]\nrequire-virtualenv = true" > ~/.config/pip/pip.conf` |
| Hash-based requirements 管理 | High | 手動確認 | `pip-compile --generate-hashes` でハッシュ付き requirements を生成 | transitive dependency も含めたハッシュ検証を導入 |
| pip-audit の定期実行 | High | 手動確認 | 既知 CVE がゼロ | `uv tool run pip-audit` を定期実行し、検出された CVE を順次対処 |
| PEP 740 attestation 確認の習慣化 | Medium | 手動確認 | 新規インストール前に attestation の有無を確認 | `uv pip install --verify-hashes` または `are-we-pep740-yet` で採用状況を確認 |

---

## JavaScript (bun / yarn)

| 項目 | 優先度 | 検査方法 | 期待状態 | FAIL時の対策 |
|------|--------|---------|---------|-------------|
| `.npmrc` の `ignore-scripts` | Critical | audit.sh | `~/.npmrc` に `ignore-scripts=true` が存在 | `echo "ignore-scripts=true" >> ~/.npmrc` を実行 |
| `.bunfig.toml` の `ignore-scripts` | Critical | audit.sh | `~/.bunfig.toml` の `[install]` セクションに `ignore-scripts = true` が存在 | `~/.bunfig.toml` に以下を追記: `[install]\nignore-scripts = true` |
| ロックファイルの厳格化 | High | 手動確認 | CI/インストール時に `--frozen-lockfile` を使用 | yarn: `yarn install --frozen-lockfile`、bun: `bun install --frozen-lockfile` をスクリプトに設定 |
| npm provenance 検証 | High | 手動確認 | 新規パッケージ追加時に `npm audit signatures` を実行 | `npm audit signatures` で Sigstore 署名を確認。署名なしパッケージは信頼性を個別評価 |
| Scoped Package による dependency confusion 防止 | Medium | 手動確認 | 内部パッケージがプライベートレジストリに限定 | `.npmrc` に `@scope:registry=<private-registry>` を設定 |

---

## Go modules

| 項目 | 優先度 | 検査方法 | 期待状態 | FAIL時の対策 |
|------|--------|---------|---------|-------------|
| `GOFLAGS` に `-mod=readonly` | Medium | audit.sh | `GOFLAGS` に `-mod=readonly` が含まれる（go コマンドがある場合のみ検査） | `.zshrc` に `export GOFLAGS="-mod=readonly"` を追加 |
| govulncheck の定期実行 | High | 手動確認 | Go プロジェクトで定期的に govulncheck を実行 | `go install golang.org/x/vuln/cmd/govulncheck@latest` 後、`govulncheck ./...` を定期実行 |
| `go mod verify` の実行 | High | 手動確認 | checksum DB と一致し、エラーなし | `go mod verify` を実行。改ざん検出時はモジュールキャッシュを削除して再取得 |
| プライベートモジュールの SSH 認証 | Medium | 手動確認 | `GONOSUMCHECK` / `GONOSUMDB` でプライベートモジュールを適切に設定 | `GOPRIVATE` 環境変数を設定し、PAT の代わりに SSH キーで認証 |

---

## mise

| 項目 | 優先度 | 検査方法 | 期待状態 | FAIL時の対策 |
|------|--------|---------|---------|-------------|
| mise を最新版に維持 | High | 手動確認 | `mise --version` が最新安定版 | `mise self-update` を実行。セキュリティ修正を含むため常に最新版を維持する |
| asdf プラグインから aqua/github バックエンドへの移行 | High | 手動確認 | `mise.toml` でシェルコードを実行する asdf プラグインを使用していない | `.mise.toml` の各ツール定義を `aqua:` または `github:` バックエンドに変更 |
| `mise trust` 実行前の内容確認 | Medium | 手動確認 | 新規クローン時に `mise.toml` の内容を確認してから `mise trust` を実行 | `cat mise.toml` で内容を確認後、問題なければ `mise trust` を実行 |
| `MISE_PARANOID=1` の検討 | Low | 手動確認 | 厳格な署名検証が必要な場合のみ設定 | `.zshrc` に `export MISE_PARANOID=1` を追加。未署名ツールが使えなくなる点に注意 |

---

## エコシステム横断

| 項目 | 優先度 | 検査方法 | 期待状態 | FAIL時の対策 |
|------|--------|---------|---------|-------------|
| Dependabot の有効化 | Critical | 手動確認 | GitHub リポジトリの Dependabot が有効 | GitHub Settings > Security > Dependabot alerts を有効化 |
| AI 提案パッケージ名の検証 | High | 手動確認 | AI が提案したパッケージ名を PyPI / npm で実在確認してからインストール | slopsquatting 対策: `uv pip index versions <pkg>` や `npm view <pkg>` で存在確認後にインストール |
| GitHub Actions のハッシュ pin | High | 手動確認 | `.github/workflows/` 内の Action 参照がコミットハッシュで固定 | `uses: actions/checkout@v4` を `uses: actions/checkout@<SHA>` 形式に変更 |
| OpenSSF Scorecard による依存先評価 | Medium | 手動確認 | 主要依存パッケージのスコアを確認済み | `https://scorecard.dev/` で依存先リポジトリを検索し、スコアが低いものは代替を検討 |
| SBOM 生成の自動化 | Low | 手動確認 | CycloneDX 形式の SBOM を定期生成 | `cyclonedx-py` や `@cyclonedx/cyclonedx-npm` で SBOM を生成し、成果物として保存 |
