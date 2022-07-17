# yarn-lock-parser

[![ActionsStatus](https://github.com/robertohuertasm/yarn-lock-parser/workflows/Build/badge.svg)](https://github.com/robertohuertasm/yarn-lock-parser/actions) [![Crates.io](https://img.shields.io/crates/v/yarn-lock-parser.svg)](https://crates.io/crates/yarn-lock-parser)

Easily parse `yarn-lock` files (v1 and v2).

## Example

```rust
use std::{error::Error, fs};
use yarn_lock_parser::{parse_str, Entry};

fn main() -> Result<(), Box<dyn Error>> {
    let yarn_lock_text = fs::read_to_string("yarn.lock")?;
    let entries: Vec<Entry> = parse_str(&yarn_lock_text)?;

    println!("{:?}", entries);

    Ok(())
}
```

## Documentation

Visit [https://docs.rs/yarn-lock-parser/](https://docs.rs/yarn-lock-parser/)

## Build

You will need [cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html), the Rust package manager.

```bash
cargo build
```

## Test

```bash
cargo test
```
