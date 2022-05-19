## Setup

```bash
$ yarn
```

## Build TypeScript bindings & example JSON files

```bash
$ dune build
```

then check `compile-tests/exXX_ts_gen.ts` and `compile-tests/exXX_examples.json`.

## Run tests

Run `$ yarn` to populate `node_modules` beforehand!!

### All tests

```bash
$ dune runtest
```

### TypeScript unit tests only

```bash
$ yarn test
```

### JSOO integration tests only

```bash
$ dune runtest jsoo-integration-tests
```

## Troubleshooting

### `dune runtest` fails with `error Command "tsc" not found` !

This is because `_build/default/with_js/node_modules` got wiped.

Populate `with_js/node_modules` to prevent this:

```bash
$ yarn
$ dune clean && dune runtest
```

### Want to clean the generated files!

This should clean all the dune & typescript artifacts:

```bash
$ dune clean
$ yarn clean
```

