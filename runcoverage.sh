#!/bin/sh

# Run this script after a test run:
# $ ./dist/build/runtests/runtests

rm main.tix

./dist/build/main/main

hpc report main --exclude=Main
hpc markup main --exclude=Main

