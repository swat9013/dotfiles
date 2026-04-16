# 独立レビュー: 目的関数テンプレート

<!-- ハブ役割: このファイルは独立レビューの目的関数集約ハブとして機能する。各スキル（plan, architect, breakdown, implement, review-fix 等）は独立レビューを委譲する際にここを参照し、レビュアーサブエージェントへ目的関数を注入する（ハブ経由注入）。正本はこのファイルのみ。スキル個別に目的関数を重複記述しない。 -->

実行前に以下の判定に従い `docs/harness-engineering-domain-model.md` の該当セクションを Read し、本レビューの目的関数として内面化する:

**レビュー対象がharness本体**（`.claude-global/`, `.claude/rules/`, `docs/` 等）**の場合**:
- `harness-engineering-domain-model.md#architecture-fitness-harness`（Architecture Fitness Harness 三項組）を主目的関数として Read する
- `harness-engineering-domain-model.md#maintainability-harness` の境界条件のみ Read し侵食禁止制約とする

**レビュー対象がアプリケーションコードの場合**:
- `harness-engineering-domain-model.md#maintainability-harness`（Maintainability Harness 三項組）を主目的関数として Read する
- `harness-engineering-domain-model.md#architecture-fitness-harness` の境界条件のみ Read し侵食禁止制約とする

すべての指摘は三項組への因果（最大化価値を損なう / 達成シグナルを変化させる / 境界条件を侵害する）で記述する。
