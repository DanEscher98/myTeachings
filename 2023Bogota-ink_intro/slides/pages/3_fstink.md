# Ink Overview

---
hideInToc: true
---

# Mensajes

```mermaid
flowchart LR
    macros["`**Macros**
    #\[ink(message)]
    - payable
    - selector
    - default
    _`"]
    macros -.-> msg([Message])
    msg -.-> q("Query: &self")
    msg ==> res([Result])
    msg -.-> t(Transaction: &mut self)
    res -.-> ok(Ok)
    res -.-> err(Err) --> Revert

    q -->|Consulta|st
    t -->|Modifica|st
    
    subgraph st [Storage]
        struct[Rust struct]
    end

```

---
hidenInToc:true
---
# Deployment en nodo local

1. Compilar

    `cargo contract build`

2. Correr nodo local

    `substrate-contracts-node`

3. Contracts UI

    `contracts-ui.substrate.io`

---
hidenInToc:true
---

Eventos 

```mermaid
flowchart TD
    wallet([Wallet])-->|TX|sc
    sc["`ink!
    - Constructores
    - Mensajes
    _`"]
    sc -->|Emite|event

    subgraph event [Evento]
        t1[Topic #1]
        t2[Topic #2]
        tn[Topic #n]
    end
    
    subgraph client
        rpc[RPC]
        idx[Indexer]
        fe[Frontend]
    end
    
    client --o|escucha|event

```

---
hideInToc: true
---
# Data flow
<img src="/img/data-flow-ink.jpeg" style="width: 100%; height: 100%; object-fit: contain; margin: 0px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);" />

---
hidenInToc: true
---
# Ink modules
<img src="/img/ink-overview.webp" style="width: 100%; height: 100%; object-fit: contain; margin: 0px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);" />

