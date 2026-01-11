# yarn-lock-parser

[![ActionsStatus](https://github.com/robertohuertasm/yarn-lock-parser/workflows/Build/badge.svg)](https://github.com/robertohuertasm/yarn-lock-parser/actions) [![Crates.io](https://img.shields.io/crates/v/yarn-lock-parser.svg)](https://crates.io/crates/yarn-lock-parser) [![Docs.rs](https://docs.rs/yarn-lock-parser/badge.svg)](https://docs.rs/yarn-lock-parser/)

A fast and reliable parser for `yarn.lock` files written in Rust. Supports all yarn.lock versions (v1, v2, and later). Parse lock files into structured data for dependency analysis, validation, and programmatic access.

## Features

- ✅ Supports all yarn.lock versions (v1, v2, and later)
- ✅ Fast and memory-efficient parsing using nom
- ✅ Comprehensive error handling with `thiserror`
- ✅ Zero-copy where possible
- ✅ Pure Rust implementation

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
yarn-lock-parser = "0.13"
```

## Quick Start

```rust
use std::{error::Error, fs};
use yarn_lock_parser::{parse_str, Entry};

fn main() -> Result<(), Box<dyn Error>> {
    let yarn_lock_text = fs::read_to_string("yarn.lock")?;
    let entries: Vec<Entry> = parse_str(&yarn_lock_text)?;

    for entry in entries {
        println!("{:?}", entry);
    }

    Ok(())
}
```

## Documentation

API documentation is available on [docs.rs](https://docs.rs/yarn-lock-parser/)

## Development

### Requirements

- [Rust](https://www.rust-lang.org/tools/install) (with `cargo`)

### Build

```bash
cargo build
```

### Test

```bash
cargo test
```

### Running Tests Verbosely

```bash
cargo test -- --nocapture
```

## License

MIT
