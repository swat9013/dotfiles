# Ghostty + Emacs 相性不具合ガイド

最終更新: 2026-02-07 | Ghostty v1.2.3 (stable) / tip (nightly) | Emacs 30.2

## 概要

GhosttyターミナルでEmacsを使用する際に発生する不具合と、その原因・対策をまとめる。

---

## 1. マウススクロールフリーズ（主要不具合）

### 症状

Ghostty上のEmacs（emacsclient -t）でマウスホイールスクロールするとフリーズし、入力を一切受け付けなくなる。Terminal.appでは発生しない。

### 根本原因: terminfoの伝播失敗

Ghosttyは独自の`xterm-ghostty` terminfoを使用し、`TERMINFO`環境変数で管理する。Emacsデーモンがlaunchd経由で起動されるとこの環境変数を継承しないため、terminfoの解決に失敗する。

| 環境 | TERM | TERMINFO |
|------|------|----------|
| Ghosttyシェル | xterm-ghostty | `/Applications/Ghostty.app/Contents/Resources/terminfo` |
| Emacsデーモン | dumb | なし |

### 発生メカニズム

```
1. emacs --daemon がGhostty外（launchd等）で起動
2. デーモンは TERMINFO 環境変数を継承しない
3. emacsclient がGhostty内から接続
4. xterm-ghostty の terminfo が解決できない
5. SGRマウスプロトコル（1006）の不整合が発生
6. マウススクロールでイベント処理がブロック → フリーズ
```

### SGRマウスプロトコルの違い

| ターミナル | kmous | プロトコル |
|-----------|-------|-----------|
| Ghostty (xterm-ghostty) | `\E[<` | SGR 1006拡張 |
| Terminal.app (xterm-256color) | `\E[M` | X10従来 |

Emacsの`xterm-mouse-mode`がSGR 1006を受信するが、terminfoが正しく解決されないためイベント処理で不整合が発生する。

---

## 2. VSync関連のフリーズ

### 症状

TUIアプリ（Emacs含む）で画面更新が止まり、操作不能になる場合がある。

### 原因

Ghosttyは macOSではvsyncがデフォルトで**有効**。無効化するとmacOS 14.4以降でカーネルパニックやパフォーマンス劣化を引き起こす可能性がある（特にDisplayLink等の外部ディスプレイ接続時）。

> [Ghostty Configuration Reference](https://ghostty.org/docs/config/reference): "Disabling vsync on macOS 14.4+ can cause kernel panics and performance issues for external displays over hardware such as DisplayLink."

### 対策

```bash
# ghostty/config
window-vsync = false
```

**注意**: `window-vsync = false`はmacOS 14.4以降のカーネルパニックリスクがある。外部ディスプレイ使用時は特に注意。フリーズが発生しない環境ではデフォルト（true）のままにすることを推奨。

---

## 3. デッドロックによるフリーズ（v1.2.3で修正済み）

### 症状

カラー変更・クエリ操作が多いプログラムで完全にハングする。Claude Codeとの長時間セッション後にタブ全体がフリーズする報告もある。

### 原因と修正

IOスレッドがカラー変更/クエリ操作でメールボックスを飽和させ、ターミナル状態ロックを保持したままデッドロックに陥るバグ。v1.2.3で段階的に修正された。

| PR | 内容 | マージ日 |
|----|------|---------|
| [#9224](https://github.com/ghostty-org/ghostty/pull/9224) | カラー変更操作のデッドロック修正 | 2025-10-15 |
| [#9270](https://github.com/ghostty-org/ghostty/pull/9270) | スクロールバー状態によるレンダラーブロック修正 | 2025-10-19 |
| [#9705](https://github.com/ghostty-org/ghostty/pull/9705) | カラースキーム報告のレースコンディション修正 | 2026-01-20 |

### 対策

Ghostty v1.2.3以降にアップデートする。

```bash
ghostty +version  # バージョン確認
```

---

## 4. BEL文字によるフリーズ（v1.2.3以降のtipで修正）

### 症状

BEL文字（0x07）を繰り返し出力するプログラム実行時にGhosttyがフリーズし、CPU 100%で応答不能になる。

### 対策

tip (nightly) チャネルにアップデート、またはv1.3.0（2026年3月リリース予定）を待つ。

> [Issue #9800](https://github.com/ghostty-org/ghostty/issues/9800) (2025-12, 修正済み)

---

## 5. ウィンドウ最大化時のフリーズ

### 症状

ウィンドウをモニタ全幅/全高に最大化すると、UI更新が停止する。入力はバッファリングされ、ウィンドウがフォーカスを失うまで反映されない。

### 原因

Ghosttyのレンダリングパイプラインとウィンドウサイズの組み合わせに起因。

### 対策

フルスクリーンではなく、わずかに小さいウィンドウサイズを使用する。

---

## 6. クリップボード連携の問題（Linux固有）

### 症状

Linux (Wayland/Hyprland) 環境で、GhosttyからEmacsへのペースト（C-y / M-y）が機能しない。EmacsからGhosttyへのコピーは正常。

### 状態

再現困難として未解決。macOSでは報告なし。

> [Discussion #7362](https://github.com/ghostty-org/ghostty/discussions/7362) (2025-05, stale)

---

## 7. Kitty Keyboard Protocol の非互換

### 症状

Kitty Keyboard Protocolを有効化しても、`M-left`が`^[b`として送信されEmacsが`M-b`と解釈する。

### 状態

調査中。Ghostty側のプロトコル実装とEmacs側の解釈の不一致の可能性。

> [Discussion #9368](https://github.com/ghostty-org/ghostty/discussions/9368) (2025-10, 議論中)

---

## 現在の対策と設定

### 対策A: TERMフォールバック（採用中）

`.zsh/aliases.zsh` の`e()`関数でTERMを上書き。

```bash
# TERM=xterm-256color: Ghosttyのxterm-ghostty terminfoがEmacsデーモンに
# 伝播しないためフォールバック（SGRマウスプロトコル不整合回避）
function e(){
    echo "[$0] emacsclient -c -t $*";
    (TERM=xterm-256color emacsclient -c -t $* ||
            (echo "[$0] emacs --daemon"; emacs --daemon &&
                 (echo "[$0] emacsclient -c -t $*"; TERM=xterm-256color emacsclient -c -t $*)) ||
            (echo "[$0] emacs $*"; emacs $*))
}
```

| 評価 | 内容 |
|------|------|
| メリット | 即座に解決、追加設定不要、安定動作 |
| デメリット | Ghostty固有機能が使えない（24bitカラー、装飾下線等） |
| リスク | 低 |

### 対策B: launchd plist修正（未実施）

Emacsデーモンの起動設定にTERMINFO環境変数を追加する。

```xml
<key>EnvironmentVariables</key>
<dict>
  <key>TERMINFO</key>
  <string>/Applications/Ghostty.app/Contents/Resources/terminfo/</string>
</dict>
```

**対象ファイル**: `$(brew --prefix)/Cellar/emacs/[version]/homebrew.mxcl.emacs.plist`

反映: `brew services restart emacs`

| 評価 | 内容 |
|------|------|
| メリット | 根本解決、Ghostty機能フル活用 |
| デメリット | Homebrewアップデートでplist上書きの可能性、カスタムplist管理が必要 |

### 対策C: terminfoインストール（未実施）

Ghosttyのterminfoをユーザーディレクトリにインストールする。

```bash
tic -x -o ~/.terminfo /Applications/Ghostty.app/Contents/Resources/terminfo/78/xterm-ghostty
```

| 評価 | 内容 |
|------|------|
| メリット | 一度の設定で全アプリケーションに有効 |
| デメリット | Ghosttyアップデート時に再インストールが必要な可能性 |

### 比較表

| 評価軸 | A: TERMフォールバック | B: launchd修正 | C: terminfoインストール |
|-------|---------------------|----------------|----------------------|
| 導入容易性 | ++ | - | + |
| メンテナンス性 | ++ | - | + |
| Ghostty機能互換 | - | ++ | ++ |
| 根本解決 | - | ++ | ++ |

---

## Emacs側の関連設定

`.emacs.d/lisp/init-core.el` でマウスサポートを有効化している。

```elisp
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)
(mouse-wheel-progressive-speed nil)  ; 加速無効
```

これらはterminfoが正しく解決されて初めて正常動作する。

---

## 今後の展望

### ncurses更新によるシステム全体の解決

xterm-ghosttyのterminfoはncurses 6.5-20241228に収録済み ([ncurses ML](https://lists.gnu.org/archive/html/bug-ncurses/2024-12/msg00035.html), 2024-12)。しかし2026-02時点でmacOSバンドルのncursesはまだ更新されていない。`brew install ncurses`で最新版を導入可能だが、システムワイドには反映されない。

### Emacs側の対応

Emacs 30.1 (2025-02) / 30.2 (2025-08) でxterm-ghosttyの直接サポートは追加されていない。Emacs 31（2026年リリース予定）の開発版でも明示的な対応は確認できない。

### Ghostty v1.3.0（2026年3月予定）

[Milestone](https://github.com/ghostty-org/ghostty/milestone/7)で追跡中。SSH用のterminfo伝播（ssh-env, ssh-terminfo）は実装済みだが、ローカルのデーモンプロセスへの伝播改善は未定。Discussion #5902では「Ghostty側でワークアラウンド不要にしてほしい」というユーザー要望がある (2025-09)。

---

## 参考資料

### 公式ドキュメント
- [Ghostty Terminfo Help](https://ghostty.org/docs/help/terminfo)
- [Ghostty Configuration Reference](https://ghostty.org/docs/config/reference)
- [Ghostty 1.2.0 Release Notes](https://ghostty.org/docs/install/release-notes/1-2-0)
- [Ghostty 1.2.3 Release Notes](https://ghostty.org/docs/install/release-notes/1-2-3)

### コミュニティ
- [Ghostty, emacsclient, and terminfo](https://skife.org/b4/ghostty-emacsclient/) (2025) - launchd plist解決策
- [Discussion #5902](https://github.com/ghostty-org/ghostty/discussions/5902) - Emacs server terminfo設定
- [Discussion #7362](https://github.com/ghostty-org/ghostty/discussions/7362) - クリップボード問題 (Linux)
- [Discussion #7405](https://github.com/ghostty-org/ghostty/discussions/7405) - マウスサポート停止問題
- [Discussion #9190](https://github.com/ghostty-org/ghostty/discussions/9190) - フリーズ問題
- [Discussion #9368](https://github.com/ghostty-org/ghostty/discussions/9368) - Kitty Keyboard Protocol

### 修正PR
- [PR #9224](https://github.com/ghostty-org/ghostty/pull/9224) - カラー操作デッドロック修正 (v1.2.3)
- [PR #9270](https://github.com/ghostty-org/ghostty/pull/9270) - スクロールバーデッドロック修正 (v1.2.3)
- [PR #9705](https://github.com/ghostty-org/ghostty/pull/9705) - カラースキーム報告レースコンディション修正

### ncurses / terminfo
- [ncurses ML: New terminfo: xterm-ghostty](https://lists.gnu.org/archive/html/bug-ncurses/2024-12/msg00035.html) (2024-12)
- [Issue #2542](https://github.com/ghostty-org/ghostty/issues/2542) - ncursesへのterminfo公開
