# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview
<!-- 1-2 sentences describing the project purpose -->
<!-- Example: Inventory management SaaS for SMBs. Provides real-time inventory tracking and automated ordering. -->

## Architecture
<!-- High-level architecture overview -->
<!-- Example: Monolith / Modular Monolith / Microservices -->
<!-- Example: Layered (Controller → Service → Repository → DB) -->

### Directory Structure
<!-- Key directories and their roles -->
<!-- Example:
- src/domain/ - Business logic
- src/api/ - API endpoints
- src/infrastructure/ - External service integration
-->

## Tech Stack
<!-- Example: TypeScript 5.x, Next.js 14, Prisma, PostgreSQL -->
- Language:
- Framework:
- Key Libraries:

## Coding Guidelines

### Design Principles
- Complexity signals a flawed problem definition. Reframe the question, not the solution.
- Build only what is needed now. Never add "future-proofing" extensibility.
- Create small units that do one thing well, then compose them.

### Implementation Rules
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

## Critical Areas
<!-- Areas requiring human review before changes -->
<!-- Example: Payment/Billing, Authentication/Authorization, Data Migration, External API Integration -->

## Prohibited Actions
<!-- Operations that are prohibited in this project -->
<!-- Example: Direct .env editing, Production config changes, Logging sensitive information -->

## Error Handling
<!-- Project error handling policy -->
<!-- Example: Use custom exception classes, Error code system, Logging rules -->

## Key Design Decisions
<!-- Summary of major technical choices and design decisions -->
<!-- Example: Using Firebase Auth (reason: reduced implementation cost), Zustand for state management -->
<!-- If separated into docs/: → Details: docs/decisions/ -->

## Common Commands
<!-- Frequently used commands during development -->
<!-- Example:
- Dev server: npm run dev
- Test: npm test
- Build: npm run build
- Lint: npm run lint
-->

## Testing Policy
<!-- Testing framework and basic policy -->
<!-- Example:
- Framework: Jest + React Testing Library
- Coverage target: 80%+
- Naming convention: describe("feature", () => { it("expected behavior", ...) })
-->

---
Last updated: YYYY-MM-DD
Target: Under 200 lines
