# devops.el: Agentic Devops

This package allows you to provision and manage infra

## Requirements

- magit
- agent shell
- Claude Code.

## Installing

```
(use-package devops
  :ensure t
  :vc (:url "https://github.com/unifica-ai/devops.el"))
``

## Project structure

- `tools.org`: a collection of commands as org LOB blocks
- `migrations/`: migration notebooks
- `incidents/`: incident notebooks
