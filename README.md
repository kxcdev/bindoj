# bindoj

## Current Roadmap:

https://github.com/kxcteam/bindoj/issues/15

## Getting Started

```bash
git clone --recursive git@github.com:kxcteam/bindoj.git

# or
git clone git@github.com:kxcteam/bindoj.git
git submodule init
git submodule update
```

```bash
opam install . --deps-only --with-test
opam install -y dune # you'd need (dune (>= 3.0))
dune build && dune runtest
```

## Developer Setup

```bash
dune build @doc
```

### Toolchain Versions

Tool | version
-----|-------
Nodejs version | 14.x (but also test against 16.x LTS && 17.x)
JSOO version | 4.0.0 (target : browser)
OCaml version | 4.12.x, 4.13.x, 4.14.x
Yarn version | 1.22.17

### NPM or Yarn?

yarn!

## Contribution Guide 
