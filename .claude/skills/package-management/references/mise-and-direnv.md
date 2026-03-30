# mise と direnv の運用

## asdf → mise 移行の経緯

**移行理由**:
- パフォーマンス: asdf のシム層は120msオーバーヘッド。mise は PATH操作で約5ms
- 統合性: mise は環境変数管理（direnv代替）+ タスクランナーを内蔵
- 互換性: `.tool-versions` をそのまま読み込むため移行コスト最小
- 採用実績: GitLab が2025年4月にデフォルト化

**移行は一方向**: asdf への逆戻りは困難。mise の fuzzy version（`lts`等）を asdf は解釈できない。

## activate 方式の選定理由

`.zshrc` でキャッシュ方式を使用（direnv/sheldon と同じパターン）。

| 方式 | 仕組み | 速度 | 採用 |
|------|--------|------|------|
| activate（キャッシュ） | 出力をファイルに保存し source | 初回のみサブプロセス | **採用** |
| activate（直接eval） | 毎回 `eval "$(mise activate zsh)"` | 毎回サブプロセス | 不採用 |
| shims | `~/.local/share/mise/shims` 経由で間接実行 | ~120ms/コマンド | 不採用 |

**キャッシュの注意点**: mise アップデート後にキャッシュ削除が必要（`rm ~/.cache/mise/activate.zsh`）。IDE のターミナル以外（GUI から直接起動されるプロセス等）では activate が効かない。そのケースでは `mise x -- command` で明示実行。

## direnv 併用の判断

mise 公式は direnv との併用を非推奨（`mise activate` が direnv の機能を包含するため）。

**それでも残置している理由**:
- 既存プロジェクトに `.envrc` が存在し、mise `[env]` への一括移行は非現実的
- direnv はプロジェクト横断的なツール。dotfiles 以外のリポジトリでも使用

**将来の方向**: 新規プロジェクトでは mise `[env]` セクションを優先。既存 `.envrc` は段階的に移行。

## .tool-versions の運用

- `~/.tool-versions` にグローバルバージョンを定義（Python, Node.js）
- プロジェクト固有バージョンは各リポジトリの `.tool-versions` または `mise.toml` で上書き
- mise は `.tool-versions` と `mise.toml` の両方を読むが、新規設定は `mise.toml` 推奨（`[env]`、`[tasks]` が使えるため）

## mise でカバーしない範囲

- システムツール（ripgrep, jq 等）→ Homebrew
- GUIアプリ → Homebrew cask
- Go のグローバルインストール → `go install`（Brewfile の go で Go 本体を管理、バイナリは `~/go/bin`）
