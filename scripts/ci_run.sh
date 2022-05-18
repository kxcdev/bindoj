#!/bin/bash
echo $ opam exec -- dune build
opam exec -- dune build

echo $ opam exec -- dune runtest
opam exec -- dune runtest
