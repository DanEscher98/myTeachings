---
theme: ./theme
class: text-center
highlighter: prism
lineNumbers: false
info: |
  ## Polkadot-Rust template
  Presentation slides for Polkadot events
drawings:
  persist: false
transition: fade-out
title: Rust Workshop
---

# Rust Workshop

Danyiel Colin

<div class="pt-12">
  <span @click="$slidev.nav.next" class="px-2 py-1 rounded cursor-pointer" hover="bg-white bg-opacity-10">
    Let's start! 🦀 <carbon:arrow-right class="inline"/>
  </span>
</div>

<div class="abs-br m-6 flex gap-2">
  <button @click="$slidev.nav.openInEditor()" title="Open in Editor" class="text-xl slidev-icon-btn opacity-50 !border-none !hover:text-white">
    <carbon:edit />
  </button>
  <a href="https://github.com/slidevjs/slidev" target="_blank" alt="GitHub"
    class="text-xl slidev-icon-btn opacity-50 !border-none !hover:text-white">
    <carbon-logo-github />
  </a>
</div>

---

# Goals of the Workshop
1. Familiarity with `cargo` workflow (TDD, deps)
2. Understand `mods` & project structure
3. Get an panoramic view of the Rust ecosystem
4. Gain intuitions when reading source code

---

# Table of contents

<Toc columns=2></Toc>

---
transition: slide-up
src: ./pages/1_introduction.md
---

---
transition: slide-up
src: ./pages/2_workflow.md
---

---
transition: slide-up
src: ./pages/3_syntax_a.md
---

---
transition: slide-up
src: ./pages/4_syntax_b.md
---

---
transition: slide-up
src: ./pages/5_features.md
---

---
transition: slide-up
src: ./pages/6_compilation.md
---

---
transition: slide-up
src: ./pages/7_exercise.md
---

---
transition: slide-up
src: ./pages/8_beyond.md
---






# A classic pointer

```mermaid
flowchart LR
    subgraph Steps
        direction TB
        step1[1 Allocate memory]
        step2[2 Assign value to memory]
        step3[3 Access value through pointer]
    end

    subgraph Variables
        ptr{ptr}
        var{value}
    end

    subgraph Memory
        data(Data)
    end

    ptr -->|points to| data
    var -->|contains| data

    step1 -->|allocate| data
    step2 -->|assign| var
    step3 -->|access| var
```

# benches

---
layout: center
class: text-center
---

# Learn More

[Documentations](https://sli.dev) · [GitHub](https://github.com/slidevjs/slidev) · [Showcases](https://sli.dev/showcases.html)
