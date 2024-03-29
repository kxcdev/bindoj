#!/bin/bash
echo ">>> uname -a"
uname -a

echo && echo ">>> OCaml --version"
opam exec -- ocamlc --version

echo && echo ">>> Node --version"
node --version

echo && echo ">>> Yarn --version"
yarn --version

echo && echo ">>> git status"
git show HEAD^..HEAD --stat || git show HEAD --stat

echo && echo ">>> git ls-files"
git ls-files
