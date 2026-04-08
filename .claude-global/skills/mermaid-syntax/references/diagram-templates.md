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

## Gantt Chart / Git Graph / Pie Chart / Quadrant Chart / User Journey / Kanban / Mindmap / Timeline / XY Chart / Sankey Diagram / Block Diagram

詳細は `diagram-templates-advanced.md` を参照。
