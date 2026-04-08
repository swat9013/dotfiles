# Mermaid 高度・ニッチ図種別テンプレート

Gantt / Git Graph / Pie Chart および使用頻度の低い図種別のテンプレート集。

## Gantt Chart

```mermaid
gantt
    title Project Timeline
    dateFormat YYYY-MM-DD
    section Design
        Requirements :a1, 2026-01-01, 10d
        Mockup       :a2, after a1, 7d
    section Development
        Backend      :crit, b1, after a2, 20d
        Frontend     :b2, after a2, 15d
    section Testing
        QA           :c1, after b1, 10d
```

## Git Graph

```mermaid
gitGraph
    commit
    branch feature
    checkout feature
    commit
    commit
    checkout main
    merge feature
    commit
```

## Pie Chart

```mermaid
pie title Distribution
    "Category A" : 40
    "Category B" : 30
    "Category C" : 20
    "Category D" : 10
```

## Quadrant Chart

```mermaid
quadrantChart
    title Priority Matrix
    x-axis Low Impact --> High Impact
    y-axis Low Effort --> High Effort
    quadrant-1 Do First
    quadrant-2 Schedule
    quadrant-3 Delegate
    quadrant-4 Eliminate
    Feature A: [0.8, 0.2]
    Feature B: [0.3, 0.7]
    Feature C: [0.6, 0.5]
```

## User Journey

```mermaid
journey
    title User Onboarding
    section Sign Up
        Visit site: 5: User
        Fill form: 3: User
        Verify email: 4: User, System
    section First Use
        Complete tutorial: 4: User
        Create first item: 5: User
```

## Kanban

```mermaid
kanban
    todo["To Do"]
        task1["Design API"]
        task2["Write tests"]
    doing["In Progress"]
        task3["Implement auth"]
    done["Done"]
        task4["Setup CI"]
```

## Mindmap

```mermaid
mindmap
    root((Project))
        Frontend
            React
            TypeScript
        Backend
            Node.js
            PostgreSQL
        Infrastructure
            Docker
            AWS
```

## Timeline

```mermaid
timeline
    title Release History
    section 2025
        Q1 : v1.0 Release
        Q3 : v2.0 Beta
    section 2026
        Q1 : v2.0 Release
        Q2 : v2.1 Patch
```

## XY Chart

```mermaid
xychart-beta
    title "Monthly Sales"
    x-axis [Jan, Feb, Mar, Apr, May]
    y-axis "Revenue (USD)" 0 --> 5000
    bar [1200, 2300, 1800, 3100, 4200]
    line [1000, 2000, 1500, 2800, 3900]
```

## Sankey Diagram

```mermaid
sankey-beta
Source A,Target X,100
Source A,Target Y,50
Source B,Target X,30
Source B,Target Z,70
```

## Block Diagram

詳細は公式ドキュメントを参照（ベータ機能のため構文変更の可能性あり）。
