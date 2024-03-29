# js-sameworld-bridge

### Labeled argument optional with default

When defining a `bridgeable_decl`, we don't want it to be an optional argument, but when calling it, it must be an optional argument.
Therefore, they are generated as optional arguments, but the default value is passed internally, so `None` (OCaml) or `undefined` (JS) are never passed.

## OCaml

### resolution strategy

The resolution_strategy determines how the referenced type_decl is generated.

- `` `no_resolution ``: Nothing.
- `` `inline_type_definition ``: Inline type generation is only possible for Alias or Polymorphic variants.
- `` `infile_type_definition ``: Types are defined in the file.

### Ident

For `Coretype.Ident`, the code is generated assuming that type `xxx` and function `xxx_{of/to}_json` can be referenced.

### type_decl

`type_decl`s referenced by the `bridgeable_decl` that `` `infile_type_definition `` is specified by `resolution_strategy` are generated.

### `Simple_interfaces` module

`` ([> `simple ], 'bridgeable_ident) bridgeable_decl ``s are generated.

Note that they are basically generated according to `method_descriptor_body`, with the only exception that if both positional & labelled arguments are empty, the module style generates them as `unit -> ret_type`.

### `Initial_registry_object_registers` module

`object_registry_decl`s whose party is `` `endemic `` are generated.

### `Concrete_bridge` module type

#### `Complex_interfaces` module

`` ([> `complex ], 'bridgeable_ident) bridgeable_decl ``s are generated.

The notes on arguments are the same as for simple interfaces.

#### `Peer_object_registry` module

`object_registry_decl`s whose party is `` `peer `` are generated.

Stored as an OCaml StringMap.

#### `Peer_objects` module

`named_object_decl`s whose party is `` `peer `` are generated.

#### `Endemic_object_registry` module

Same as `Initial_registry_object_registers`.

#### `endemic_full_bridge` variable

Variable for the setup process with JavaScript.

Internally, the following type definition is expected.

```typescript
{
    readonly setupCalled: () => boolean;
    readonly setup: (fullBridgeOfJs: { readonly instance: ConcreteBridgeOfJs }) => void;
}
```

### `Concrete_bridge_interfaces` module

Only interfaces are generated.

### Full Bridge

A (functor) module type definition is generated to generate concrete_bridge. Based on the `named_object_decl` and `object_registry_decl` parties in the given `sameworld_objintf`, one of the following four types of full_bridge is generated.

* `Setup_less_full_bridge`: Nothing included.
* `Endemic_setup_only_full_bridge`: Only `` `peer `` is included.
* `Peer_setup_only_full_bridge`: Only `` `endemic `` is included.
* `Dual_setup_full_bridge`: Both `` `peer `` and `` `endemic `` are included.

If `named_object_decl`s whose party is ``endemic `` are included, they are generated as functor parameters.

## Js_of_ocaml

### type_decl

Currently, the json encoder/decoder generated by `Bindoj_gen` is used for type_decl conversion.

## TypeScript

Basically, the data is converted by the code generated on the OCaml side, so only the type definition and some implementation for the full bridge is generated as TypeScript code.

The basic structure is the same as OCaml, but differs in some respects.

### resolution stragety

- `` `import_location ``: Types are imported from the specified file.
- `` `inline_type_definition ``: Types are generated inline.
- `` `infile_type_definition ``: Types are defined in the file.

### The arguments of the generated method
In TS, convert to a list of types to be taken by the arguments and then delete all trailing `unit`s. (Naturally, this may result in an empty list, in which case a type such as `foo : () => RetType` is generated.)

### Runtime implementation

For the generation of full bridges, types and functions defined in [objintf.ts](../../../with_js/public-packages/runtime/src/objintf.ts) are used.
