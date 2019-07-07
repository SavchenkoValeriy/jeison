#!/usr/bin/env bash

set -e

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l jeison.el -l jeison-tests.el -f ert-run-tests-batch-and-exit
