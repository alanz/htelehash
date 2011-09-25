#!/bin/sh

# Run this script after a test run:
# $ ./dist/build/runtests/runtests

rm runtests.tix

./dist/build/runtests/runtests

hpc report runtests --exclude=runtests

hpc markup runtests --exclude=runtests

