<p align="center">
  <a href="http://dev.stephendiehl.com/hask/">
    <img src="http://dev.stephendiehl.com/hask/img/title.png"/>
  </a>
</p>

[![MIT License](http://img.shields.io/badge/license-mit-blue.svg)](https://github.com/sdiehl/wiwinwlh/blob/master/LICENSE)
[![Build Status](https://travis-ci.org/sdiehl/wiwinwlh.svg?branch=master)](https://travis-ci.org/sdiehl/wiwinwlh)

Read Online:

* [**HTML**](http://dev.stephendiehl.com/hask/)
* [**Screen PDF**](http://dev.stephendiehl.com/hask/tutorial.pdf)
* [**Printable PDF**](http://dev.stephendiehl.com/hask/tutorial_print.pdf)
* [**EPUB**](http://dev.stephendiehl.com/hask/tutorial.epub)
* [**Kindle MOBI**](http://dev.stephendiehl.com/hask/tutorial.mobi)
* [**Example Code**](https://github.com/sdiehl/wiwinwlh/tree/master/src)

If you'd like a physical copy of the text you can either print it out for
yourself (see Printable PDF) or purchase one from a publisher. The price is
at-cost since the text is open source and free.

* [**Blurb Publisher**](https://www.blurb.co.uk/b/9958091-what-i-wish-i-knew-when-learning-haskell)

The current published version is:

```
Date: February 16, 2020
Git: d429c2e21b9636cffa27a8d4f063644b8bcecf1a
```

Contributing
------------

If you want to submit a fix for a typo or fix edit the following Markdown file
which generates all other targets.

[tutorial.md](./tutorial.md)

Then submit a Pull Request on Github describing your change.

Source code is directly included from the `src` directory into the resulting
document by the preprocessor. To include a new sample edit the example source
code directly. When adding a new Haskell file, add it to the `ci` script inside
of the folder so it is run in the continuous integration script.

Source Code
-----------

Chapter Code Examples:

* [01-basics/            ](https://github.com/sdiehl/wiwinwlh/tree/master/src/01-basics/)
* [02-monads/            ](https://github.com/sdiehl/wiwinwlh/tree/master/src/02-monads/)
* [03-monad-transformers/](https://github.com/sdiehl/wiwinwlh/tree/master/src/03-monad-transformers/)
* [04-extensions/        ](https://github.com/sdiehl/wiwinwlh/tree/master/src/04-extensions/)
* [05-laziness/          ](https://github.com/sdiehl/wiwinwlh/tree/master/src/05-laziness/)
* [06-prelude/           ](https://github.com/sdiehl/wiwinwlh/tree/master/src/06-prelude/)
* [07-text-bytestring/   ](https://github.com/sdiehl/wiwinwlh/tree/master/src/07-text-bytestring/)
* [08-applicatives/      ](https://github.com/sdiehl/wiwinwlh/tree/master/src/08-applicatives/)
* [09-errors/            ](https://github.com/sdiehl/wiwinwlh/tree/master/src/09-errors/)
* [10-advanced-monads/   ](https://github.com/sdiehl/wiwinwlh/tree/master/src/10-advanced-monads/)
* [11-quantification/    ](https://github.com/sdiehl/wiwinwlh/tree/master/src/11-quantification/)
* [12-gadts/             ](https://github.com/sdiehl/wiwinwlh/tree/master/src/12-gadts/)
* [13-lambda-calculus/   ](https://github.com/sdiehl/wiwinwlh/tree/master/src/13-lambda-calculus/)
* [14-interpreters/      ](https://github.com/sdiehl/wiwinwlh/tree/master/src/14-interpreters/)
* [15-testing/           ](https://github.com/sdiehl/wiwinwlh/tree/master/src/15-testing/)
* [16-type-families/     ](https://github.com/sdiehl/wiwinwlh/tree/master/src/16-type-families/)
* [17-promotion/         ](https://github.com/sdiehl/wiwinwlh/tree/master/src/17-promotion/)
* [18-generics/          ](https://github.com/sdiehl/wiwinwlh/tree/master/src/18-generics/)
* [19-numbers/           ](https://github.com/sdiehl/wiwinwlh/tree/master/src/19-numbers/)
* [20-data-structures/   ](https://github.com/sdiehl/wiwinwlh/tree/master/src/20-data-structures/)
* [21-ffi/               ](https://github.com/sdiehl/wiwinwlh/tree/master/src/21-ffi/)
* [22-concurrency/       ](https://github.com/sdiehl/wiwinwlh/tree/master/src/22-concurrency/)
* [23-graphics/          ](https://github.com/sdiehl/wiwinwlh/tree/master/src/23-graphics/)
* [24-parsing/           ](https://github.com/sdiehl/wiwinwlh/tree/master/src/24-parsing/)
* [25-streaming/         ](https://github.com/sdiehl/wiwinwlh/tree/master/src/25-streaming/)
* [26-data-formats/      ](https://github.com/sdiehl/wiwinwlh/tree/master/src/26-data-formats/)
* [27-web/               ](https://github.com/sdiehl/wiwinwlh/tree/master/src/27-web/)
* [28-databases/         ](https://github.com/sdiehl/wiwinwlh/tree/master/src/28-databases/)
* [29-ghc/               ](https://github.com/sdiehl/wiwinwlh/tree/master/src/29-ghc/)
* [30-languages/         ](https://github.com/sdiehl/wiwinwlh/tree/master/src/30-languages/)
* [31-template-haskell/  ](https://github.com/sdiehl/wiwinwlh/tree/master/src/31-template-haskell/)
* [32-cryptography       ](https://github.com/sdiehl/wiwinwlh/tree/master/src/32-cryptography/)
* [33-categories/        ](https://github.com/sdiehl/wiwinwlh/tree/master/src/33-categories/)
* [34-time/              ](https://github.com/sdiehl/wiwinwlh/tree/master/src/34-time/)

Compiling
---------

If you'd like to build the documents locally you'll need to compile the
preprocessor run either use Nix, Stack or Cabal to compile the preprocessor
executable in the cabal file.

Stack:

```bash
$ stack install
$ stack exec make
```

Cabal:

```bash
$ cabal install
$ cabal exec make
```

For Nix:

```bash
$ make run-shell
$ ghc --make includes.hs
$ make
```

After this is built you can build anyone of the available target outputs.

**LaTeX**

You will need the whole XeTeX suite to compile.

```bash
$ apt-get install texlive texlive-xetex texlive-extra texlive-extra-utils
$ make pdf
$ make print
```

**HTML**

```bash
$ make html
```

**Microsoft Word**

```bash
$ make docx
```

**Ebook**

```bash
$ make epub
```

Continuous Integration
----------------------

The book and example code is kept stable under continuous integration. The
script `src/ci` will run through all example code and compile it against the
`example.cabal` file of the given chapter. All bounds for dependencies are
specified in the cabal file and ensure that the given code examples are up to
date.

The CI is run in Github under TravisCI on every pull request.

To run the integration suite manually run:

```bash
$ cd src && ./ci
```

License
-------

Copyright © 2009-2020 Stephen Diehl

This code included in the text is dedicated to the public domain. You can copy,
modify, distribute and perform the code, even for commercial purposes, all
without asking permission.

You may distribute this text in its full form freely, but may not reauthor or
sublicense this work. Any reproductions of major portions of the text must
include attribution.

The software is provided "as is", without warranty of any kind, express or
implied, including But not limited to the warranties of merchantability, fitness
for a particular purpose and noninfringement. In no event shall the authors or
copyright holders be liable for any claim, damages or other liability, whether
in an action of contract, tort or otherwise, Arising from, out of or in
connection with the software or the use or other dealings in the software.
