---
title: Spring Course Setup
author: Danyiel Colin
date: 11-February-2022
---

## Introduction

In order to automate the environment setup, installation of required
tools and the compilation process, a `bash` and a `Powershell` scripts
are provided.

## Steps for the `Windows Subsystem for Linux`

## `VirtualBox` on Windows host

## Steps for native Linux system

The script will perform the following steps:

1. Install:
    - The **make** tool
    - The newest **Python** version
    - The **Haskell** interpreter and compiler
    - Optional: **Pandoc**, to generate a `PDF` of this markup file
2. 

In a Linux terminal, navigate to the directory where these files are
located and type the following commands:

```bash
git clone https://github.com/DanEscher98/Courses.git
chmod u+x setup.sh  # Change to executable mode
sudo ./setup.sh     # To install packages permission is needed
```
