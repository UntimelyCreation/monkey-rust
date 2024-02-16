# monkey-rust

An implementation of the Monkey programming language written in Rust, from Thorsten Ball's [*Writing an Interpreter in Go*](https://interpreterbook.com/) and [*Writing a Compiler in Go*](https://compilerbook.com) books.

I started this project to learn about how programming languages work by building an interpreter, a REPL, and eventually a compiler as well as a virtual machine.

I chose Rust instead of Go mainly for practice, and also to avoid copy-pasting the code directly from the book. Rust also seems better suited for this kind of low-level work.

I try to stay as close as possible to Thorsten's Go implementation for now as I'm still learning. I intend to add additional features later on.


## Usage

### Build and run tests

```bash
cargo build
cargo test
```

### Run the binaries

#### REPL

```bash
cargo run --release --bin repl
```

#### Benchmark

The benchmark program measures the time required to execute a recursive Fibonacci calculation in Monkey. A single argument must be passed, indicating which engine to use for the computation: "vm" (compiler and VM) or "eval" (interpreter).

```bash
cargo run --release --bin benchmark -- vm
```


## License

Distributed under the MIT License.
