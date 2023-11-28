# monkey-rust

An implementation of the Monkey programming language written in Rust, from Thorsten Ball's [*Writing an Interpreter in Go*](https://interpreterbook.com/) book.

I started this project to learn about how programming languages work by building an interpreter, a REPL, and eventually a compiler as well as a virtual machine.

I chose Rust instead of Go mainly for practice, and also to avoid copy-pasting the code directly from the book. Rust also seems better suited for this kind of low-level work.

I try to stay as close as possible to Thorsten's Go implementation for now as I'm still learning. I intend to refactor the code into more idiomatic Rust and add additional features later on.


## Usage

### Build and run tests

```bash
cargo build
cargo test
```

### Run the REPL

```bash
cargo run --release --bin repl
```


## License

Distributed under the MIT License.
