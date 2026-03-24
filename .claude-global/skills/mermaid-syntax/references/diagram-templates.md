# Mermaid 図種別構文テンプレート

## Contents

- Flowchart
- Sequence Diagram
- Class Diagram
- State Diagram
- ER Diagram
- Architecture Diagram
- Gantt Chart
- Git Graph
- Pie Chart
- Quadrant Chart
- User Journey
- Kanban
- Mindmap
- Timeline
- XY Chart
- Sankey Diagram
- Block Diagram

## Flowchart

```mermaid
flowchart TD
    A["開始"] --> B{"判定?"}
    B -->|Yes| C["処理A"]
    B -->|No| D["処理B"]
    C --> E["終了"]
    D --> E
```

方向: `TD`(上→下), `LR`(左→右), `BT`(下→上), `RL`(右→左)

ノード形状:
- `A["矩形"]` — プロセス
- `B{"ダイヤモンド"}` — 判定
- `C(("円"))` — 接続点
- `D[("円筒")]` — データベース
- `E[["サブルーチン"]]` — サブプロセス
- `F(["スタジアム"])` — 端子

矢印:
- `-->` 実線矢印
- `---` 実線（矢印なし）
- `-.->` 点線矢印
- `==>` 太線矢印
- `-->|label|` ラベル付き

## Sequence Diagram

```mermaid
sequenceDiagram
    participant C as Client
    participant S as Server
    participant D as DB

    C->>S: POST /api/data
    activate S
    S->>D: INSERT query
    D-->>S: OK
    S-->>C: 201 Created
    deactivate S

    Note over S,D: 非同期処理
```

矢印:
- `->>` 実線（同期）
- `-->>` 点線（応答）
- `-x` 実線（失敗）
- `-)` 実線（非同期）

構文要素:
- `participant A as Label` — エイリアス
- `activate/deactivate` — ライフライン
- `Note over A,B: text` — ノート
- `alt/else/end` — 条件分岐
- `loop text/end` — ループ
- `par/and/end` — 並列処理

## Class Diagram

```mermaid
classDiagram
    class Animal {
        +String name
        +int age
        +makeSound() String
    }
    class Dog {
        +String breed
        +bark() void
    }
    Animal <|-- Dog

    class Cat {
        +purr() void
    }
    Animal <|-- Cat
```

関係:
- `<|--` 継承
- `*--` コンポジション
- `o--` 集約
- `-->` 依存
- `..>` 実現

## State Diagram

```mermaid
stateDiagram-v2
    [*] --> Idle
    Idle --> Processing: start()
    Processing --> Done: complete()
    Processing --> Error: fail()
    Error --> Idle: retry()
    Done --> [*]

    state Processing {
        [*] --> Validating
        Validating --> Executing
        Executing --> [*]
    }
```

## ER Diagram

```mermaid
erDiagram
    CUSTOMER ||--o{ ORDER : places
    ORDER ||--|{ LINE_ITEM : contains
    PRODUCT ||--o{ LINE_ITEM : "is in"

    CUSTOMER {
        int id PK
        string name
        string email
    }
    ORDER {
        int id PK
        int customer_id FK
        date created_at
    }
```

カーディナリティ:
- `||--||` 1対1
- `||--o{` 1対多
- `}o--o{` 多対多

## Architecture Diagram

```mermaid
architecture-beta
    group cloud(cloud)[Cloud]

    service api(server)[API Server] in cloud
    service db(database)[Database] in cloud
    service cache(disk)[Cache] in cloud

    api:R --> L:db
    api:B --> T:cache
```

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
