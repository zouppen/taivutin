<!--- -*- mode: markdown -*- -->

# Finnish word detector

This was written to help detecting Finnish street names from given
text document. It kind-of-works but lacks cabal build file and some
comments and such.

This is written for Tuomas Räsänen's municipality activity detection
tool. Produced word list may be used to dig up information from public
sources (such as: lautakunnan pöytäkirjat, valtuustolistat etc.)

## Requirements

In addition to Haskell Platform, Pandoc libraries are needed for
reading the markdown file in examples/ directory. To install in Ubuntu
12.04, run:

    apt-get install haskell-platform libghc-pandoc-dev

## Running

To try the examples:

	ghc --make Main.hs
    ./Main examples/streets_in_jyvaskyla.txt
