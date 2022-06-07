# bindoj

## Current Roadmap:

https://github.com/kxcteam/bindoj/issues/15

## Getting Started

Please install the following tools beforehand.

- [dune](https://dune.build/)
  - Follow the official [installation instructions](https://dune.build/install), or
  - (for KXC members) the [KXC internal instructions](https://stackoverflow.com/c/kxcteam/questions/21/22#22) for OCaml setup (including dune)
- yarn
  - step1: Install [nodejs](https://nodejs.org/en/)
  - step2: run `npm install -g yarn`

```bash
git clone --recursive git@github.com:kxcteam/bindoj.git

# or
git clone git@github.com:kxcteam/bindoj.git
git submodule init
git submodule update
```

```bash
make setup

make build # or `dune build`
make runtest # or `dune runtest`
```

## Developer Setup

```bash
make doc # or `dune build @doc`
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
