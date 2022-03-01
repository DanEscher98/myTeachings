# Automate Embedded Development Setup

## Introduction

This script has the purpose of automating the work area for the
embedded systems area. It eliminates the process of installing tools
one by one, configures an `Ubuntu` virtual machine and installs tools
inside the virtual machine as well. In order to automate the
environment setup, installation of required tools and the compilation
process, a `bash` and a `PowerShell` scripts are provided.

## Instructions

The script will perform the following steps:

1.  Install:
    -   The **make** tool
    -   The newest **Python** version

In a Linux terminal, navigate to the directory where these files are
located and type the following commands:

``` bash
git clone https://github.com/DanEscher98/Courses.git
chmod u+x setup.sh # Change to executable mode
sudo ./setup.sh # To install packages permission is needed
```

## Software that will be installed on the Windows host

-   Microsoft `PowerShell` 7
-   Microsoft `Windows Terminal`
-   Oracle `VirtualBox`
-   Hashicorp `Vagrant`
-   Axosoft `GitKraken`
-   `git` and `gsudo`
