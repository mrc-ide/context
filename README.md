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

# Packages

Coordinating package installation on remote machines turns out to be a bit horrible, especially when you only want packages installed if they are not already there (hence all the constructs like `if (!require(pkg)) install.packages(pkg)...` that tend to litter scripts.  This situation is compounded if some packages are to come from non-CRAN repositories (e.g., unstable versions, research code, etc).

To help this, `context` allows specifying a `package_sources` object that can include packages from:

* `drat` repositories
* github (using the devtools `user/repo[/subdir][@ref]` syntax
* bitbucket
* local packages (useful for private reposoitories as there's no need to deal with access rights)

This is done by creating a local, ad-hoc, drat repository that the target machine can use.

There is some overlap here between `context` and tools like [`packrat`](https://rstudio.github.io/packrat/) and [`rbundler`](https://github.com/opower/rbundler) but the focus here is less on constructing exact versioned environments and more on a lightweight way of bootstrapping an environment and dealing with the possibility of packages from different architectures or R versions coexsting within a common filesystem tree with a minimal amount of reinstalling.

# Generalising

Come up with a general way of specifying all the required bits so that we can reuse this in different contexts; `callr`, `rrq`, `experimentr`, `remake` all do variants of this but all store things in different ways.  Something that generalised all of them would be great.  This probably just requires store/retrieve methods for the classed objects that are returned.

## Installation

```r
# install.packages("drat")
drat:::add("mrc-ide")
install.packages("context")
```
