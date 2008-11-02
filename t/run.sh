#!/bin/sh

runhaskell -XEmptyDataDecls -XOverloadedStrings -XPatternGuards t/Main.hs
