# rmosh

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![Crates.io](https://img.shields.io/crates/v/rmosh.svg)](https://crates.io/crates/rmosh)

rmosh is a Rust implementation of the Mosh Scheme interpreter, which was originally written in C++. It supports all of the features in R7RS small and R6RS standards. Please note that rmosh is currently in its alpha stage.

## Repository

The project is hosted at [https://github.com/higepon/mosh/tree/master/rmosh](https://github.com/higepon/mosh/tree/master/rmosh).

## Crate

The rmosh crate is available at [https://crates.io/crates/rmosh/](https://crates.io/crates/rmosh/).

## Trying rmosh

For most Rust users, the easiest way to try rmosh is by running the following command:

```
cargo install rmosh
```

## Build Instructions

To build rmosh from source, follow these steps:

1. Clone the repository:
   ```
   git clone https://github.com/higepon/mosh.git
   cd mosh/rmosh
   ```

2. Build the project using Cargo:
   ```
   cargo build --release
   ```

## Installation

To install rmosh after building from source, follow these steps:

1. Install the binary with the following command:
   ```
   cargo install --path .
   ```

2. Make sure the installation directory is in your PATH.

## Running Tests

To run all the tests, simply execute the following command in the project directory:

```
make test
```

## Limitations and Contributions

Although rmosh passes the R6RS and R7RS tests, there are still some features to be implemented. You may find `todo!()` macros throughout the codebase, indicating areas where your help would be appreciated. Please feel free to submit pull requests to improve the project.
