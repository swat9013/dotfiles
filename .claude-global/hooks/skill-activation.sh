#!/bin/bash
# UserPromptSubmit hook: スキル強制評価
# 参考: Scott Spenceの実験データ（forced eval → 84% vs baseline 20%）
# コミットメントメカニズム: YES/NOを明示させてからActivateさせる構造

cat <<'EOF'
BEFORE RESPONDING, you MUST complete this skill evaluation:

Step 1 - EVALUATE: Review available skills. For each potentially relevant skill, state YES/NO with one-line reason.
Step 2 - ACTIVATE: If any YES, use Skill() tool NOW before anything else.
Step 3 - IMPLEMENT: Only after Step 1-2 are complete, proceed with the task.

MANDATORY: Skipping evaluation and jumping to implementation is a critical failure.
EOF
