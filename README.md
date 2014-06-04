
[![Build Status](https://travis-ci.org/alanz/htelehash.png)](https://travis-ci.org/alanz/htelehash)



How to build
------------

Library:

cabal clean && cabal configure && cabal build

Tests:

cabal clean && cabal configure -fbuildtests && cabal build

Running the tests

./dist/build/runtests/runtests


Running
--------

Best current example is tft.hs

Running with any argument will turn on verbose logging.

A representation of the messages is always written to file line.log


Changes
-------

