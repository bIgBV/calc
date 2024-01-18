# Pratt Parser for Simple Arithmetic Expressions

This project implements a basic Pratt parser for parsing simple arithmetic
expressions. Pratt parsing is a top-down operator precedence parsing technique
that allows for parsing expressions with varying levels of operator precedence.

## Getting Started

### Prerequisites

Make sure you have Rust installed on your system. If not, you can [install Rust](https://www.rust-lang.org/learn/get-started).

### Installation

Clone the repository to your local machine:

```bash
git clone https://github.com/bIgBV/calc
```

### Usage

Navigate to the project directory and run the following command to build and run the parser:

```bash
cargo run
```

This will build the project and execute the parser on a set of sample arithmetic expressions.

## Parser Details

The Pratt parser in this project is designed to handle basic arithmetic expressions containing the following operators:

- Addition (+)
- Subtraction (-)
- Multiplication (*)
- Division (/)
- Grouping (`()`)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.