# iucc

Haskell implementation of the [Essentials of Compilation book by Jeremy
Siek][book].

Current status of each language can be found here:

| Language | Parser | Interpreter | Assembler |
|:--------:|:------:|:-----------:|:--------:|
| $L_{int}$    | ✅ | ✅ | ❌ |
| $L_{var}$    | 💻 | ❌ | ❌ |

**KEY** : ✅ implemented, ❌ not implemented, 💻 current focus

## Development

This project uses [Stack] to build, make sure you have it installed before proceeding.

### Building

You can build the compiler by simply running:

```shell
stack build
```

### Testing

You can run the test suite on every change with the following command:

```shell
stack build --test --coverage --file-watch
```

### Profiling

Not yet supported, but eventually in the plan.

### Running the compiler

Use stack to execute the compiler after it's built

```shell
stack execute -- iucc --file path/to/sample/file.rkt
```

Or if you have stack's bin directory in your path you can run `install`:

```shell
stack install && iucc --file path/to/sample/file.rkt
```

[book]: https://mitpress.mit.edu/97802620"47760/essentials-of-compilation/
[Stack]: https://docs.haskellstack.org/en/stable/
