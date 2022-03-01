# Automate Embedded Development Setup

## Introduction

This script has the purpose of automating the work area for the
embedded systems area. It eliminates the process of installing tools
one by one, configures an `Ubuntu` virtual machine and installs tools
inside the virtual machine as well. In order to automate the
environment setup, installation of required tools and the compilation
process, a `bash` and a `PowerShell` scripts are provided.

## Specifications

The script will perform the following steps:

1. On the Windows Host:
    - Download and install `winget`, the Windows package manager
    - Using `winget` it will install:
        - Microsoft `PowerShell` 7
        - Microsoft `Windows Terminal`
        - Oracle `VirtualBox`
        - Hashicorp `Vagrant`
        - Axosoft `GitKraken`
        - `git` and `gsudo`
    - Add global variables to the `$PROFILE` of `$PowerShell`
2. Inside the Virtual Machine:
    - Install: git, make, gcc, neovim, python3, pip3
    - Install: valgrind, nasm, clang
    - Set a default configuration for neovim
    - Configure `gh` the cli-tool of Github.

## Instructions

In a Linux terminal, navigate to the directory where these files are
located and type the following commands:

``` bash
git clone https://github.com/DanEscher98/Courses.git
chmod u+x setup.sh # Change to executable mode
sudo ./setup.sh # To install packages permission is needed
svn export https://github.com/DanEscher98/ClasesMaterial/trunk/Setups/jdubuntu
```
