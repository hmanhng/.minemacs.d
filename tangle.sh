#!/usr/bin/env bash
function main () {
  emacs --batch -l org "$1" -f org-babel-tangle
}
main "readme.org"
