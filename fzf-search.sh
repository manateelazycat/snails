#!/usr/bin/sh

cat $1 | fzf -e -f "$2"
