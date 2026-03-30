# Ghostty + Emacs 相性不具合リファレンス

最終更新: 2026-03-28 | Ghostty v1.3.1 | Emacs 30.2

## 目次

1. [マウススクロールフリーズ（主要不具合）](#1-マウススクロールフリーズ主要不具合)
2. [VSync関連のフリーズ](#2-vsync関連のフリーズ)
3. [デッドロックによるフリーズ（v1.2.3で修正済み）](#3-デッドロックによるフリーズv123で修正済み)
4. [BEL文字によるフリーズ（v1.2.3以降tipで修正済み）](#4-bel文字によるフリーズv123以降tipで修正済み)
5. [ウィンドウ最大化時のフリーズ](#5-ウィンドウ最大化時のフリーズ)
6. [クリップボード連携の問題（Linux固有）](#6-クリップボード連携の問題linux固有)
7. [Kitty Keyboard Protocolの非互換](#7-kitty-keyboard-protocolの非互換)
8. [現在の対策と設定](#8-現在の対策と設定)
9. [Emacs側の関連設定](#9-emacs側の関連設定)
10. [今後の展望](#10-今後の展望)
11. [参考資料](#11-参考資料)

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

GhosttyはmacOSではvsyncがデフォルトで**有効**。無効化するとmacOS 14.4以降でカーネルパニックやパフォーマンス劣化を引き起こす可能性がある（特にDisplayLink等の外部ディスプレイ接続時）。

### 対策

```bash
# ghostty/config
window-vsync = false
```

**注意**: macOS 14.4以降のカーネルパニックリスクあり。外部ディスプレイ使用時は特に注意。フリーズが発生しない環境ではデフォルト（true）推奨。

---

## 3. デッドロックによるフリーズ（v1.2.3で修正済み）

IOスレッドがカラー変更/クエリ操作でメールボックスを飽和させデッドロックに陥るバグ。v1.2.3で修正済み。**v1.2.3以降にアップデートすれば解決。**

---

## 4. BEL文字によるフリーズ（v1.2.3以降tipで修正済み）

BEL文字（0x07）を繰り返し出力するプログラムでGhosttyがCPU 100%でフリーズするバグ。v1.3.x以降で修正済み。**v1.3.1以降にアップデートすれば解決。**

---

## 5. ウィンドウ最大化時のフリーズ

### 症状

ウィンドウをモニタ全幅/全高に最大化すると、UI更新が停止する。入力はバッファリングされ、ウィンドウがフォーカスを失うまで反映されない。

### 対策

フルスクリーンではなく、わずかに小さいウィンドウサイズを使用する。

---

## 6. クリップボード連携の問題（Linux固有）

Linux (Wayland/Hyprland) 環境で、GhosttyからEmacsへのペースト（C-y / M-y）が機能しない。EmacsからGhosttyへのコピーは正常。macOSでは報告なし。再現困難として未解決。

---

## 7. Kitty Keyboard Protocolの非互換

Kitty Keyboard Protocolを有効化しても、`M-left`が`^[b`として送信されEmacsが`M-b`と解釈する。調査中。

---

## 8. 現在の対策と設定

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

Emacsデーモンの起動plistに`TERMINFO`環境変数を追加する方法。メリットは根本解決・Ghostty機能フル活用だが、Homebrewアップデートでplist上書きリスクあり。

### 対策C: terminfoインストール（未実施）

`tic -x -o ~/.terminfo` でユーザーディレクトリにterminfoをインストールする方法。一度の設定で全アプリに有効だが、Ghosttyアップデート時に再インストールが必要な場合あり。

### 比較表

| 評価軸 | A: TERMフォールバック | B: launchd修正 | C: terminfoインストール |
|-------|---------------------|----------------|----------------------|
| 導入容易性 | ++ | - | + |
| メンテナンス性 | ++ | - | + |
| Ghostty機能互換 | - | ++ | ++ |
| 根本解決 | - | ++ | ++ |

---

## 9. Emacs側の関連設定

`.emacs.d/lisp/init-core.el` でマウスサポートを有効化。terminfoが正しく解決されて初めて正常動作する。

```elisp
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)
(mouse-wheel-progressive-speed nil)  ; 加速無効
```

---

## 10. 今後の展望

- **ncurses更新**: xterm-ghosttyのterminfoはncurses 6.5-20241228に収録済みだが、v1.3.1時点でmacOSバンドルのncursesはまだ未更新。`brew install ncurses`で最新版導入可能だがシステムワイドには反映されない。
- **Emacs側対応**: Emacs 30.x でxterm-ghosttyの直接サポートは未追加。Emacs 31（2026年リリース予定）でも明示的な対応は未確認。
- **Ghosttyのローカルデーモンへのterminfo伝播改善**: SSH用の伝播（ssh-env, ssh-terminfo）は実装済みだが、ローカルデーモンへの改善は未定。

---

## 11. 参考資料

- [Ghostty Terminfo Help](https://ghostty.org/docs/help/terminfo)
- [Ghostty Configuration Reference](https://ghostty.org/docs/config/reference)
- [Ghostty, emacsclient, and terminfo](https://skife.org/b4/ghostty-emacsclient/) (2025) — launchd plist解決策
- [Discussion #5902](https://github.com/ghostty-org/ghostty/discussions/5902) — Emacs server terminfo設定
- [Discussion #7405](https://github.com/ghostty-org/ghostty/discussions/7405) — マウスサポート停止問題
- [Discussion #9368](https://github.com/ghostty-org/ghostty/discussions/9368) — Kitty Keyboard Protocol
