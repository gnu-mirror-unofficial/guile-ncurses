#!/bin/sh

# This script opens up a gdb session for a single guile-ncurses test

# The splitter characters used for word splitting after expansion
IFS="$(printf '\n\t')"

# Exit immediately if a command returns a non-zero status
set -e

# Make sure there is exactly one parameter
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 TEST" >&2
    exit 1
fi
if ! [ -e ./"$1" ]; then
    echo "$1 not found" >&2
    exit 1
fi
if ! [ -f ./"$1" ]; then
    echo "$1 not a regular file" >&2
    exit 1
fi

# env (set by configure)
top_builddir="/home/mike/guile"
XDG_CACHE_HOME=${top_builddir}/cache
export XDG_CACHE_HOME
exec ./uninstalled-env ../libtool --mode=execute \
    gdb --args guile -L .. "$1"
