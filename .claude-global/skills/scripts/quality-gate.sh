#!/usr/bin/env bash
# quality-gate.sh - Lint/Test/Build コマンドの検出・実行・結果報告
#
# Usage: quality-gate.sh [--lint-cmd="CMD"] [--test-cmd="CMD"] [--build-cmd="CMD"] [project_dir]
#
# オプションなしの場合、設定ファイルから自動検出する。
# --*-cmd で明示指定した場合はそちらを優先（implementation.md等から読み取った値）。

# set -e は意図的に省略: run_gate() 内で exit_code を手動捕捉するため
set -uo pipefail

# --- オプション解析 ---
LINT_CMD=""
TEST_CMD=""
BUILD_CMD=""
PROJECT_DIR=""

for arg in "$@"; do
  case "$arg" in
    --lint-cmd=*) LINT_CMD="${arg#--lint-cmd=}" ;;
    --test-cmd=*) TEST_CMD="${arg#--test-cmd=}" ;;
    --build-cmd=*) BUILD_CMD="${arg#--build-cmd=}" ;;
    --help|-h)
      echo "Usage: quality-gate.sh [--lint-cmd=\"CMD\"] [--test-cmd=\"CMD\"] [--build-cmd=\"CMD\"] [project_dir]"
      exit 0
      ;;
    -*) echo "Unknown option: $arg" >&2; exit 1 ;;
    *) PROJECT_DIR="$arg" ;;
  esac
done

PROJECT_DIR="${PROJECT_DIR:-.}"
cd "$PROJECT_DIR" || { echo "ERROR: Cannot cd to $PROJECT_DIR" >&2; exit 1; }

# --- プロジェクト種別検出 ---
detect_project_type() {
  if [[ -f "package.json" ]]; then
    echo "node"
  elif [[ -f "pyproject.toml" ]]; then
    echo "python"
  elif [[ -f "Cargo.toml" ]]; then
    echo "rust"
  elif [[ -f "go.mod" ]]; then
    echo "go"
  elif [[ -f "Makefile" ]]; then
    echo "make"
  else
    echo "unknown"
  fi
}

# --- Node.js コマンド検出 ---
detect_node_commands() {
  if [[ -z "$LINT_CMD" ]]; then
    local has_lint
    has_lint=$(node -e "const p=require('./package.json'); console.log(p.scripts?.lint ? 'yes' : 'no')" 2>/dev/null || echo "no")
    [[ "$has_lint" == "yes" ]] && LINT_CMD="npm run lint"
  fi

  if [[ -z "$TEST_CMD" ]]; then
    local has_test
    has_test=$(node -e "const p=require('./package.json'); console.log(p.scripts?.test ? 'yes' : 'no')" 2>/dev/null || echo "no")
    [[ "$has_test" == "yes" ]] && TEST_CMD="npm test"
  fi

  if [[ -z "$BUILD_CMD" ]]; then
    local has_build
    has_build=$(node -e "const p=require('./package.json'); console.log(p.scripts?.build ? 'yes' : 'no')" 2>/dev/null || echo "no")
    [[ "$has_build" == "yes" ]] && BUILD_CMD="npm run build"
  fi
}

# --- Python コマンド検出 ---
detect_python_commands() {
  if [[ -z "$LINT_CMD" ]]; then
    if grep -q '\[tool\.ruff\]' pyproject.toml 2>/dev/null; then
      LINT_CMD="ruff check ."
    elif grep -q '\[tool\.flake8\]' pyproject.toml 2>/dev/null || [[ -f ".flake8" ]]; then
      LINT_CMD="flake8 ."
    fi
  fi

  if [[ -z "$TEST_CMD" ]]; then
    if grep -q '\[tool\.pytest\]' pyproject.toml 2>/dev/null || [[ -f "pytest.ini" ]]; then
      TEST_CMD="pytest"
    fi
  fi
}

# --- Rust コマンド検出 ---
detect_rust_commands() {
  [[ -z "$LINT_CMD" ]] && LINT_CMD="cargo fmt --check && cargo clippy -- -D warnings"
  [[ -z "$TEST_CMD" ]] && TEST_CMD="cargo test"
  [[ -z "$BUILD_CMD" ]] && BUILD_CMD="cargo build"
}

# --- Go コマンド検出 ---
detect_go_commands() {
  [[ -z "$LINT_CMD" ]] && LINT_CMD="go vet ./..."
  [[ -z "$TEST_CMD" ]] && TEST_CMD="go test ./..."
  [[ -z "$BUILD_CMD" ]] && BUILD_CMD="go build ./..."
}

# --- Makefile コマンド検出 ---
detect_make_commands() {
  if [[ -z "$LINT_CMD" ]] && grep -qE '^lint\s*:' Makefile 2>/dev/null; then
    LINT_CMD="make lint"
  fi
  if [[ -z "$TEST_CMD" ]] && grep -qE '^test\s*:' Makefile 2>/dev/null; then
    TEST_CMD="make test"
  fi
  if [[ -z "$BUILD_CMD" ]] && grep -qE '^build\s*:' Makefile 2>/dev/null; then
    BUILD_CMD="make build"
  fi
}

# --- コマンド実行 ---
run_gate() {
  local label="$1"
  local cmd="$2"

  if [[ -z "$cmd" ]]; then
    echo "[$label] SKIP (command not found)"
    return 0
  fi

  local output
  local exit_code
  # eval: パイプやリダイレクトを含むコマンド文字列を正しく実行するため
  # 呼び出し元はスキル内部のみ（外部入力は想定しない）
  output=$(eval "$cmd" 2>&1) && exit_code=0 || exit_code=$?

  if [[ $exit_code -eq 0 ]]; then
    echo "[$label] PASS"
    return 0
  else
    echo "[$label] FAIL"
    echo "--- ${label} Errors ---"
    echo "$output" | tail -30
    echo "---"
    return 1
  fi
}

# --- メイン ---
PROJECT_TYPE=$(detect_project_type)

# 明示指定がないコマンドのみ自動検出
case "$PROJECT_TYPE" in
  node)   detect_node_commands ;;
  python) detect_python_commands ;;
  rust)   detect_rust_commands ;;
  go)     detect_go_commands ;;
  make)   detect_make_commands ;;
esac

# Makefile はフォールバックとして常にチェック（他の種別でもMakefileがある場合）
if [[ "$PROJECT_TYPE" != "make" ]] && [[ -f "Makefile" ]]; then
  detect_make_commands
fi

# コマンドが1つも見つからない場合
if [[ -z "$LINT_CMD" ]] && [[ -z "$TEST_CMD" ]] && [[ -z "$BUILD_CMD" ]]; then
  echo "=== Quality Gate ==="
  echo "Project: $PROJECT_TYPE"
  echo ""
  echo "WARNING: No lint/test/build commands detected."
  echo "Specify commands explicitly: quality-gate.sh --lint-cmd=\"CMD\" --test-cmd=\"CMD\""
  echo ""
  echo "GATE: SKIP"
  exit 0
fi

echo "=== Quality Gate ==="
echo "Project: $PROJECT_TYPE"
[[ -n "$LINT_CMD" ]] && echo "Lint: $LINT_CMD"
[[ -n "$TEST_CMD" ]] && echo "Test: $TEST_CMD"
[[ -n "$BUILD_CMD" ]] && echo "Build: $BUILD_CMD"
echo ""

FAILED=0

run_gate "LINT" "$LINT_CMD" || FAILED=1
run_gate "TEST" "$TEST_CMD" || FAILED=1
run_gate "BUILD" "$BUILD_CMD" || FAILED=1

echo ""
if [[ $FAILED -eq 0 ]]; then
  echo "GATE: PASS"
else
  echo "GATE: FAIL"
  exit 1
fi
