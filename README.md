# context

[![Build Status](https://travis-ci.org/dide-tools/context.png?branch=master)](https://travis-ci.org/dide-tools/context)

> Contexts for evaluating R expressions

This package organises collecting (and recreating) the context of an R expression to replay later.  At this point, the package is hard-coded to use only filesystem storage, but soon I'll generalise that to allow interfacing with things like `rrqueue` which follow similar ideas.

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

### Environments

Local and global environments may be saved by the contexts when using `auto=TRUE`.  These will be saved into `<path>/environments/<md5>` where `<md5>` is the md5 sum of the saved file (content addressable).  Note that the global environment is saved as an *image* while the local environment is serialised as a single object.

This may do slightly weird things with complicated interactions between local and global objects and their environments, in which cases users should not use the automatic context generation.  Things like C/C++ pointers, Rcpp/RcppR6 reference objects will also be badly affected.

# Generalising

Come up with a general way of specifying all the required bits so that we can reuse this in different contexts; `callr`, `rrqueue`, `experimentr`, `remake` all do variants of this but all store things in different ways.  Something that generalised all of them would be great.  This probably just requires store/retrieve methods for the classed objects that are returned.

## Installation

```r
devtools::install_github("dide-tools/context")
```
