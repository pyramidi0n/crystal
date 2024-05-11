# Crystal

A Lispy static site generator using Spinneret, Lass, and Markdown.

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Project Structure](#project-structure)
4. [Basic Usage](#basic-usage)
5. [Starting a New Project](#starting-a-new-project)
6. [Deployment](#deployment)
7. [Links](#links)
8. [Patches](#patches)
9. [License](#license)

## Overview

Crystal is a simple static site generator that aims to be quite Lispy. Both pages
and styles are expressed in Common Lisp DSLs, while blog posts can be written in
Markdown.

While Crystal supports arbitrary stylesheets and static assets, it doesn't ship
with any themes; the assumption is that a Crystal user is also interested in
designing her own website. This could change over time, however.

## Installation

Crystal is available on [Ultralisp](https://ultralisp.org/) and is easy to
install using [Quicklisp](https://www.quicklisp.org/beta/).

Add the Ultralisp repository:

```lisp
CL-USER> (ql-dist:install-dist "http://dist.ultralisp.org/")
```

Install Crystal:

```lisp
CL-USER> (ql:quickload :crystal)
```

## Project Structure

The contents of a Crystal site live in the `site` directory.

Pages are written using [Spinneret](https://github.com/ruricolist/spinneret)
and reside in `site/pages.lisp`. Each page template is a Common Lisp function
that returns a string containing HTML5.

Blog posts are optional and reside in `site/posts/`. Posts are written using
Markdown.

Static assets such as images, fonts, and the like reside in `site/static/`; any
subdirectory structure may be used for static assets. Stylesheets are written
using [Lass](https://github.com/Shinmera/LASS) and reside in `site/styles/`.
Here, no subdirectories are expected.

The site structure is specified in `site/config.lisp`:

```lisp
(page-routes
  ("/" index)
  ("/about/" about))

(static-routes-prefix "/static/")
(styles-routes-prefix "/styles/")

(post-routes-prefix "/posts/")
(post-template post)
```

Routes are declared with the `page-routes` macro. A route consists of a path
and a symbol naming a page template function. The `post-template` macro takes
a symbol naming a page template function; Markdown is passed into this template
when each post is generated. The other macros specify path prefixes for
various assets after the site is generated.

## Basic Usage

Generating and previewing a Crystal site is straightforward:

```lisp
CL-USER> (require :crystal)

CL-USER> (crystal:generate)

CL-USER> (crystal:start-preview)
```

Then direct a web browser to `http://localhost:5000`.

The preview may be shut down with:

```lisp
CL-USER> (crystal:stop-preview)
```

## Starting a New Project

While Crystal ships with a project skeleton, it's usually desirable to duplicate
the whole system for any new website.

For example, assuming the Crystal repository resides in
`~/quicklisp/dists/ultralisp/software/pyramidi0n-crystal`:

```bash
$ cp ~/quicklisp/dists/ultralisp/software/pyramidi0n-crystal ~/new-site

$ mv ~/new-site/crystal.asd ~/new-site/new-site.asd
```

Make this new repository available to ASDF, e.g. on Linux:

```bash
$ ln -s ~/new-site ~/.local/share/common-lisp/source/
```

Then, edit the source code so that ASDF understands this is a new system:

In `~/new-site/new-site.asd`:

```lisp
(defsystem "crystal"
...)
```

becomes

```lisp
(defsystem "new-site"
...)
```

In `~/new-site/src/main.lisp`:

```lisp
(defparameter *system-path* (namestring (asdf:system-source-directory :crystal)))
```

becomes

```lisp
(defparameter *system-path* (namestring (asdf:system-source-directory :new-site)))
```

Thereafter:

```lisp
CL-USER> (require :new-site)

CL-USER> (crystal:generate)

CL-USER> (crystal:start-preview)

CL-USER> (crystal:stop-preview)
```

and so forth. Though this new site's code resides in a new ASDF system, the
`crystal` package is still used, unless that package is otherwise changed.

## Deployment

Crystal's generated output resides in `www`.

Deployment is as simple as copying the contents of that directory to a remote
host.

## Links

* [Repository](https://sr.ht/~pyramidion/crystal/)

## Patches

Patches are welcome.

## License

Crystal is licensed under the two-clause BSD license.

See LICENSE.
