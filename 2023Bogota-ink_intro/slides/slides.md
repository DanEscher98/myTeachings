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

# El rol de ink! en Polkadot

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

# Objetivos del Workshop
1. Breve introducción a Rust
2. Overview de `ink!`
3. Crear y deployar un primer `smart-contract`

---

# Table of contents

<Toc columns=2></Toc>

---
transition: slide-up
src: ./pages/1_defink.md
---

---
transition: slide-up
src: ./pages/2_rustintro.md
---

---
transition: slide-up
src: ./pages/3_fstink.md
---

[Documentations](https://sli.dev) · [GitHub](https://github.com/slidevjs/slidev) · [Showcases](https://sli.dev/showcases.html)
