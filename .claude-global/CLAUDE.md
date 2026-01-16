# Claude Code Guidelines

## Coding Philosophy

### Design Principles
- Complexity signals a flawed problem definition. Reframe the question, not the solution.
- Build only what is needed now. Never add "future-proofing" extensibility.
- Create small units that do one thing well, then compose them.

### Implementation Guidelines
- Name things to reveal intent.
- When in doubt, optimize for readability.
- Fix root causes, not symptoms. No ad-hoc workarounds.
- No implicit fallbacks. Handle errors explicitly.
- No boolean arguments. Don't branch behavior via parameters.
- Avoid lazy optionals. Force callers to express intent.

### Structural Design
- Choose the simplest solution (KISS).
- Consolidate code with the same responsibility (DRY).
- Keep statements in a function at the same abstraction level (SLAP).
- One class/module, one responsibility (SRP).

## Interaction Rules
- Ask questions proactively when anything is unclear.
- Present options with recommendations and rationale.
- Before generating a response, ask about any missing context—no matter how minor.
- Always use `AskUserQuestion` tool when proposing options or seeking clarification.

## Other
- Execute tasks in parallel whenever possible.

## User Context

### 環境制約
- macOS (Darwin) メイン環境
- Zsh + Sheldon（プラグインマネージャー）
- Homebrew でパッケージ管理

### 開発スタイル
- rm → rmtrash で誤削除防止
- fzy によるインタラクティブ選択
- emacsclient + デーモンモード

### dotfiles制約
- ~/.dotfiles をgitで管理
- シンボリックリンク方式で設定配布
