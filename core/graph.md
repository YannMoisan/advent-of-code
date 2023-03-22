# Directed but not acyclic graph

Not a DAG 1 -> 3 -> 2 -> 1

```mermaid
graph LR
    0 --> 1
    0 --> 4
    0 --> 5
    1 --> 3
    1 --> 4
    2 --> 1
    3 --> 2
    3 --> 4
```
