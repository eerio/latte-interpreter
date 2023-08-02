# Latte Interpreter in Haskell

![Haskell Logo](https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png)

This repository contains an interpreter for the [Latte](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2011/Latte/) programming language, implemented in Haskell. The Latte language is a simple, statically-typed, imperative language with functional features. This project was developed as part of the course at the University of Warsaw (MIMUW).

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Getting Started](#getting-started)
- [Usage](#usage)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

## Introduction

The Latte Interpreter is designed to execute programs written in the Latte programming language. It takes Latte source code as input and runs it, producing the corresponding output. The interpreter performs lexical analysis, parsing, and static type checking, followed by the execution of the input program. It is built using Haskell, leveraging the power and expressiveness of the functional programming paradigm.

What do I personally like in this implementation? The types! Please take a look at this:
https://github.com/eerio/latte-interpreter/blob/0f80c0cff471a4c5a941c2dda57667bc3a5f2eb1/app/Interpreter.hs#L56-L80
Beautiful, isn't it? The nontrivial thing here is the type of the [monadic transformer](https://en.wikipedia.org/wiki/Monad_transformer) (`IM a`) - as we know, monadic transformers aren't commutative, so that the type itself had to be somehow engineered. A different type could also work, but the rest of the implementation wouldn't be as elegant as now.


## Features

- **Lexical Analysis:** Tokenizes the input Latte source code, identifying keywords, literals, and other language elements.
- **Parsing:** Builds an Abstract Syntax Tree (AST) from the tokenized input using a parser implemented with Parsec library.
- **Static Type Checking:** Performs type checking and scope analysis to detect potential errors in the input program.
- **Interpretation:** Executes the input Latte program, producing the output based on the defined semantics.

## Getting Started

To get started with the Latte Interpreter, follow these steps:

1. **Prerequisites:** Ensure you have GHC 8.8.4 (Glasgow Haskell Compiler) installed on your system.

2. **Clone the repository:** Clone this repository to your local machine using the following command:

```
git clone https://github.com/eerio/latte-interpreter 
```

3. **Build the Interpreter:** Navigate to the project directory and build the interpreter using `make`:

```
cd latte-interpreter/app
make
```

## Usage

To use the Latte Interpreter, run the executable generated after building the project:

```
./interpreter good/03-fibonacci-assign.lt
```

The interpreter will process the input file, perform the necessary analysis, and execute the program, displaying the output on the console.

## Examples

To help you get started, this repository includes some example Latte programs in the `app/good/` directory (these are correct programs) and in the `app/bad` directory (these are the programs which should be rejected by the static type checker, the lexical analyser or throw a well-defined error during execution). You can use them to test the interpreter and observe its behavior with different inputs.

## Contributing

Contributions to the Latte Interpreter are welcome! If you find any bugs, issues, or have suggestions for improvements, please open an issue or submit a pull request. Make sure to follow the established coding style and guidelines.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

We hope this Latte Interpreter helps you in understanding the Latte language and its inner workings. Feel free to explore the code, experiment with the interpreter, and adapt it for your specific needs. If you have any questions or need further assistance, don't hesitate to reach out.

Happy coding! 😊

