# Latte Interpreter

![Latte Logo](https://example.com/latte_logo.png)

This repository contains an interpreter for the Latte programming language, implemented in Haskell. The Latte language is a simple, statically-typed, imperative language with functional features. This project was developed as part of the course at the University of Warsaw (MIMUW).

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Getting Started](#getting-started)
- [Usage](#usage)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

## Introduction

The Latte Interpreter is designed to execute programs written in the Latte programming language. It takes Latte source code as input and runs it, producing the corresponding output. The interpreter performs lexical analysis, parsing, and semantic analysis, followed by the execution of the input program. It is built using Haskell, leveraging the power and expressiveness of the functional programming paradigm.

## Features

- **Lexical Analysis:** Tokenizes the input Latte source code, identifying keywords, literals, and other language elements.
- **Parsing:** Builds an Abstract Syntax Tree (AST) from the tokenized input using a parser implemented with Parsec library.
- **Semantic Analysis:** Performs type checking and scope analysis to detect potential errors in the input program.
- **Interpretation:** Executes the input Latte program, producing the output based on the defined semantics.

## Getting Started

To get started with the Latte Interpreter, follow these steps:

1. **Prerequisites:** Ensure you have GHC (Glasgow Haskell Compiler) and Cabal installed on your system.

2. **Clone the repository:** Clone this repository to your local machine using the following command:

```
git clone https://github.com/your-username/latte-interpreter.git
```

3. **Build the Interpreter:** Navigate to the project directory and build the interpreter using Cabal:

```
cd latte-interpreter
cabal build
```


4. **Run Tests:** Before using the interpreter, it is recommended to run the test suite to ensure everything is working correctly:

```cabal test```

## Usage

To use the Latte Interpreter, run the executable generated after building the project. The executable is typically located in the `dist/build/latte-interpreter/` directory.

To interpret a Latte program, provide the path to the Latte source file as an argument:

```
./latte-interpreter path/to/your/latte_program.lat
```

The interpreter will process the input file, perform the necessary analysis, and execute the program, displaying the output on the console.

## Examples

To help you get started, this repository includes some example Latte programs in the `examples/` directory. You can use them to test the interpreter and observe its behavior with different inputs.

## Contributing

Contributions to the Latte Interpreter are welcome! If you find any bugs, issues, or have suggestions for improvements, please open an issue or submit a pull request. Make sure to follow the established coding style and guidelines.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

We hope this Latte Interpreter helps you in understanding the Latte language and its inner workings. Feel free to explore the code, experiment with the interpreter, and adapt it for your specific needs. If you have any questions or need further assistance, don't hesitate to reach out.

Happy coding! 😊

