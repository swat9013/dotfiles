#!/bin/bash
# security-audit/scripts/audit.sh
# パッケージマネージャのセキュリティ設定を検査するスクリプト
# 技術要件: macOS BSD互換、個別チェックは独立実行、カラー出力対応

# --- カラー設定 ---
if [ -t 1 ]; then
  C_PASS='\033[0;32m'
  C_FAIL='\033[0;31m'
  C_SKIP='\033[0;33m'
  C_RESET='\033[0m'
  C_WARN='\033[0;35m'
  C_BOLD='\033[1m'
else
  C_PASS=''
  C_FAIL=''
  C_SKIP=''
  C_WARN=''
  C_RESET=''
  C_BOLD=''
fi

# --- カウンター ---
PASS=0
FAIL=0
SKIP=0
WARN=0

# --- クリーンアップ ---
_AUDIT_TMPFILES=""
_audit_cleanup() {
  for _f in $_AUDIT_TMPFILES; do
    rm -f "$_f"
  done
}
trap _audit_cleanup EXIT

# --- ヘルパー関数 ---
pass() {
  PASS=$((PASS + 1))
  printf "${C_PASS}[PASS]${C_RESET} %s\n" "$1"
}

fail() {
  FAIL=$((FAIL + 1))
  printf "${C_FAIL}[FAIL]${C_RESET} %s\n" "$1"
}

skip() {
  SKIP=$((SKIP + 1))
  printf "${C_SKIP}[SKIP]${C_RESET} %s\n" "$1"
}

warn() {
  WARN=$((WARN + 1))
  printf "${C_WARN}[WARN]${C_RESET} %s\n" "$1"
}

# --- 環境変数チェック ---
printf "\n${C_BOLD}=== 環境変数チェック ===${C_RESET}\n"

# UV_REQUIRE_VIRTUALENV
if [ "${UV_REQUIRE_VIRTUALENV:-}" = "true" ]; then
  pass "UV_REQUIRE_VIRTUALENV — true に設定済み"
else
  fail "UV_REQUIRE_VIRTUALENV — 未設定または true でない（現在値: '${UV_REQUIRE_VIRTUALENV:-（未設定）}'）"
fi

# HOMEBREW_VERIFY_ATTESTATIONS
if [ "${HOMEBREW_VERIFY_ATTESTATIONS:-}" = "true" ]; then
  pass "HOMEBREW_VERIFY_ATTESTATIONS — true に設定済み"
else
  fail "HOMEBREW_VERIFY_ATTESTATIONS — 未設定または true でない（現在値: '${HOMEBREW_VERIFY_ATTESTATIONS:-（未設定）}'）"
fi

# GOFLAGS (-mod=readonly)
# Go開発者かどうかを判定（go コマンドの有無で判断）
if command -v go > /dev/null 2>&1; then
  if echo "${GOFLAGS:-}" | grep -qF -- '-mod=readonly'; then
    pass "GOFLAGS — -mod=readonly を含む（現在値: '${GOFLAGS:-}'）"
  else
    fail "GOFLAGS — -mod=readonly が含まれていない（現在値: '${GOFLAGS:-（未設定）}'）"
  fi
else
  skip "GOFLAGS — go コマンドが見つからないため Go 開発者ではないと判断してスキップ"
fi

# --- 設定ファイルチェック ---
printf "\n${C_BOLD}=== 設定ファイルチェック ===${C_RESET}\n"

# ~/.config/pip/pip.conf
PIP_CONF="$HOME/.config/pip/pip.conf"
if [ -f "$PIP_CONF" ]; then
  if grep -qE 'require-virtualenv[[:space:]]*=[[:space:]]*true' "$PIP_CONF" 2>/dev/null; then
    pass "pip.conf — $PIP_CONF に require-virtualenv = true を確認"
  else
    fail "pip.conf — $PIP_CONF は存在するが require-virtualenv = true が含まれていない"
  fi
else
  fail "pip.conf — $PIP_CONF が存在しない（pypi.md の手順1が未実施）"
fi

# ~/.npmrc (ignore-scripts=true)
NPMRC="$HOME/.npmrc"
if [ -f "$NPMRC" ]; then
  if grep -qE 'ignore-scripts[[:space:]]*=[[:space:]]*true' "$NPMRC" 2>/dev/null; then
    pass ".npmrc — $NPMRC に ignore-scripts=true を確認"
  else
    fail ".npmrc — $NPMRC は存在するが ignore-scripts=true が含まれていない"
  fi
else
  fail ".npmrc — $NPMRC が存在しない（Shai-Hulud蠕虫攻撃クラスへの対策が未実施）"
fi

# ~/.bunfig.toml ([install] セクションに ignore-scripts 設定)
BUNFIG="$HOME/.bunfig.toml"
if [ -f "$BUNFIG" ]; then
  # [install] セクション内に ignore-scripts = true が設定されているか確認
  # TOML形式: ignore-scripts = true (前後スペース許容)
  if grep -qE 'ignore-scripts[[:space:]]*=[[:space:]]*true' "$BUNFIG" 2>/dev/null; then
    pass ".bunfig.toml — $BUNFIG に ignore-scripts = true を確認"
  else
    fail ".bunfig.toml — $BUNFIG は存在するが [install] セクションに ignore-scripts = true がない"
  fi
else
  fail ".bunfig.toml — $BUNFIG が存在しない（bun の postinstall スクリプト対策が未実施）"
fi

# --- Homebrew チェック ---
printf "\n${C_BOLD}=== Homebrew チェック ===${C_RESET}\n"

if command -v brew > /dev/null 2>&1; then
  # brew tap の結果を一時ファイルに保存してから処理（サブシェル変数の消失を避ける）
  _BREW_TAP_TMP="${TMPDIR:-/tmp}/audit_brew_tap_$$"
  _AUDIT_TMPFILES="$_AUDIT_TMPFILES $_BREW_TAP_TMP"
  brew tap 2>/dev/null > "$_BREW_TAP_TMP"

  TAP_TOTAL=0
  THIRD_PARTY_TAPS=""

  while IFS= read -r tap; do
    [ -z "$tap" ] && continue
    TAP_TOTAL=$((TAP_TOTAL + 1))
    case "$tap" in
      homebrew/*) ;;
      *) THIRD_PARTY_TAPS="$THIRD_PARTY_TAPS $tap" ;;
    esac
  done < "$_BREW_TAP_TMP"
  rm -f "$_BREW_TAP_TMP"

  # サードパーティ tap の数を計算
  TP_COUNT=0
  for t in $THIRD_PARTY_TAPS; do
    TP_COUNT=$((TP_COUNT + 1))
  done

  if [ "$TP_COUNT" -eq 0 ]; then
    pass "Homebrew tap — サードパーティ tap なし (公式 homebrew/* のみ, tap 総数: $TAP_TOTAL)"
  else
    warn "Homebrew tap — tap 総数: $TAP_TOTAL, サードパーティ tap: $TP_COUNT 個 (手動確認要)"
    printf "           サードパーティ tap 一覧:\n"
    for t in $THIRD_PARTY_TAPS; do
      printf "             - %s\n" "$t"
    done
  fi
else
  skip "Homebrew tap — brew コマンドが見つからないためスキップ"
fi

# --- サマリー ---
printf "\n${C_BOLD}--- Summary ---${C_RESET}\n"
printf "${C_PASS}PASS: %d${C_RESET}  ${C_FAIL}FAIL: %d${C_RESET}  ${C_WARN}WARN: %d${C_RESET}  ${C_SKIP}SKIP: %d${C_RESET}\n" \
  "$PASS" "$FAIL" "$WARN" "$SKIP"

# 終端判定キー
if [ "$FAIL" -gt 0 ]; then
  printf "\nGATE: FAIL\n"
  exit 1
fi
printf "\nGATE: PASS\n"
exit 0
