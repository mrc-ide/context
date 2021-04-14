# context

<!-- badges: start -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![R build status](https://github.com/mrc-ide/context/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/context/actions)
<!-- badges: end -->

> Contexts for evaluating R expressions

This package organises collecting (and recreating) the context of an R expression to replay later.

## Concepts

### Task

A task is an **expression** to be evaluated in a **context**.  When evaluated it will create a **result**.

### Contexts

These are the ingredients to create an environment (in rrqueue they're called environments but I think that context is a little ambiguous).  A context does imply an environment, but several environments might go into a context.

These will be saved at `<path>/contexts/<id>` where `<id>` is an identifier that we'll generate to be unique to a context.

A context includes:

* a set of packages to install and load
* a set of R files to `source`, or a global environment to create
* a local environment to create

In addition, the expression may have local variables.

## Installation

```r
# install.packages("drat")
drat:::add("mrc-ide")
install.packages("context")
```
