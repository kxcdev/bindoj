#!/bin/bash
set -e

echo $ opam exec -- dune gen
opam exec -- dune build @dune-gen --auto-promote

echo $ opam exec -- dune build
opam exec -- dune build

echo $ opam exec -- dune runtest
opam exec -- dune runtest
