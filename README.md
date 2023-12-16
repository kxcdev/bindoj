Bindoj ::= Binding-OCaml with Joy!
=================================

Bindoj -- A generative datatype toolkit for OCaml centric workflows.

## DISCLAIMER

* This project is at an early stage.
  - We are doing the actual development in a separate private repository.
  - The are still a lot of works and design discussions going on internally.
  - A detailed documentation may not be available yet.
  - The current API is unstable, and is subject to change.
* As such, we **do not accept contributions** for the time being.
  - All pull requests other than internal contributations will be summarily closed.
  - Feature requests may or may not be respected, according to our internal design goals.
  - Feel free to open an issue. We will work on a fix internally if appropriate.

## User Guides

[kxcdev/bindoj-template](https://github.com/kxcdev/bindoj-template) contains
an example project, in which you are ready to try bindoj without the hussle.

### Installation

bindoj is not yet available in the OPAM repository, so you would need to
use one of the following options:

* Use dune's `vendored_dirs` feature.
  - `bindoj-template` uses this to install bindoj.
  ```bash
  $ mkdir vendors
  $ git submodule add https://github.com/kxcdev/bindoj.git vendors/bindoj
  $ git submodule update --init --recursive
  $ echo (vendored_dirs vendors) >> dune
  ```
* Use `opam pin`.
  ```bash
  $ opam pin add kxclib git@github.com:kxcinc/kxclib-ocaml.git
  $ opam pin add bindoj git@github.com:kxcdev/bindoj.git
  ```

### Libraries

- `bindoj.runtime` ... A runtime library required to run the generated code.
- `bindoj.base` ... A base library to be used in various code generators described below.
  - This contains the following sub-libraries:
    - `bindoj.runtime`
    - `bindoj.typedesc` ... Contains the AST for defining datatypes.
- `bindoj.gen` ... Code generators for OCaml.
  - Capable of generating the followings:
    - OCaml type definition.
    - Reflection (as in Java/C#) function for the generated type.
    - [JSON Schema](https://json-schema.org/) definition.
    - JSON encoder/decoder for the generated type.
- `bindoj.gen_ts` ... Code generators for TypeScript.
  - Capable of generating the following:
    - TypeScript type definition.
    - Case analyzer function for the generated variant types.
- `bindoj.apidir_shared` ... Contains the AST for describing RESTful APIs.
  - We are currently working on an OpenAPI definition generator.
- `bindoj.openapi` ... A generic binding library for [OpenAPI](https://www.openapis.org/) 3.0.
  - Unlike the above libraries, this does not depend on the other parts of bindoj.
  - There is a plan to split this into a separate repository.

### Usage

Since bindoj is still at an early stage, a complete usage guide and tutorial is not yet available.

We are working on the documentation website, and several draft articles are available.
See the [doc/tests_src](doc/tests_src) directory:
- [ocaml_datatype_basic.md](doc/tests_src/ocaml_datatype_basic.md)
  describes how to define simple datatypes, and how to generate OCaml code from them.
- [ocaml_datatype_polymorphic-variant.md](doc/tests_src/ocaml_datatype_polymorphic-variant.md)
  describes how to define a polymorphic variant with bindoj's configuration feature.
- [ocaml_json_codec.md](doc/tests_src/ocaml_json_codec.md)
  describes how to generate JSON encoders and decoders from the above definitions.

See also [kxcdev/bindoj-template](https://github.com/kxcdev/bindoj-template) for a working example.

## Developer Guides

**Note:** as mentioned in [DISCLAIMER](#disclaimer), we don't accept third party contributions for the time being.

The following guide is for the internal developers of KXC, and for those who understand the above
and still want to experiment with this tool on their own responsibility.

### Toolchain Versions

We use the following tools to develop bindoj:

Tool | version
-----|-------
[OCaml](https://ocaml.org/) | 4.13.x, 4.14.x 5.0.x
[dune](https://dune.build/) | 3.x
[js\_of\_ocaml](https://github.com/ocsigen/js_of_ocaml) | 4.x.x
[Node.js](https://nodejs.org/) | 18.x LTS (but also test against 16.x LTS && 20.x)
[Yarn](https://yarnpkg.com/) | 1.22.19

To check for other packages, see the [dune-project](dune-project) and [with_js/package.json](with_js/package.json) files.
### Setup

1. Install the following tools:
    - OCaml and dune
      - Follow the official installation instructions or
      - (for KXC members) the [KXC internal instructions](https://stackoverflow.com/c/kxcteam/questions/21/22#22).
    - Node.js
      - Follow the official installation instructions.
    - yarn
      - Run `$ npm install -g yarn`.
2. Clone this repository:
    ```bash
    $ git clone --recursive git@github.com:kxcdev/bindoj.git

    # or
    $ git clone git@github.com:kxcdev/bindoj.git
    $ git submodule init
    $ git submodule update
    ```
3. Run `make setup` to install OCaml & JS dependencies.

### `make` commands

Use the followings to build, generate files, run tests, or generate docs.
```bash
$ make build # or `dune build`
$ make gen # or `dune build @gen`
$ make test # or `dune runtest`
$ make doc # or `dune build @doc`
```

#### Brows docs

Because the coverage test takes time to run, `doc-serve` does not run `coverage`.
Therefore, if you want to view the result of the coverage test, you must run `make coverage` each time.

```bash
$ make coverage
$ make doc-serve
```

### Code formatting

We don't have strict code formatting rules (yet), but we have a rule on the source code banner.

Run `make audit` to check if your newly-added code has a proper banner.

It will instruct you to add a banner if missing, or request you to run `$ make promote-audit` to autofix an outdated or ill-formatted banner.

## License

This software is licensed under the Apache License version 2.0. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

This project is currently developed under the funding provided by AnchorZ Inc. to satisfy its needs in its product development workflow.
