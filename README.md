# C0 Compiler
![Haskell](https://img.shields.io/badge/haskell-%235D4F85?style=for-the-badge&logo=haskell)

- [About](#about)
  - [Commands Accepted](#commands-accepted)
- [Development toolchain](#development-toolchain)
- [Executing](#executing)
- [Contributors](#contributors)

## About
A basic compiler that reads [C0 language](https://c0.cs.cmu.edu/docs/c0-reference.pdf) [^1] code and generates a code in Assembly for MIPS architecture.

[^1]: A simplified version of C language created for teaching programming created by University Carnegie Mellon. See [C0 Reference Guide](https://c0.cs.cmu.edu/docs/c0-reference.pdf).

### Commands accepted
  The following commands and block of commands are accepted by this simple C0 compiler:
  - Basic types (int, bool) and constants such as `true`, `false` and integer numbers.
  - String type
  - Comments in line or block multiline: `//` or `/* multiline comment */`
  - Arithmetic expressions: `+`, `-`, `*`, `/` and `%`
  - Variable declarations, and variable simple attributions of value: `variable_name = expression`
  - Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`
  - Conditional executors:
    ```
    if(expression)
      //single line instruction or instruction block
    ```
    ```
    if(expression)
      //single line instruction or instruction block
    else
      //single line instruction or instruction block
    ```
  - Instruction blocks:
    ```
    {
      //multiple lines of instructions
    }
    ```
  - Cycles `for` or `while`:
    ```
    while(expression)
      //single line instruction or instruction block
    ```
  - Function definitions with argument parameters and possible return of a value
  - Functions for IO of integers: `scan_int()`, `print_int()`
  - Printing function for string: `print_str()`
  - Flux control: `break` and `continue`
  - Logical operators with *short-circuit* evaluation: `!`, `&&` and `||`
  
## Development toolchain
  - [Haskell](https://www.haskell.org/get-started/)
  - [Cabal](https://www.haskell.org/cabal/) for building the application.
  - [Alex](https://haskell-alex.readthedocs.io/en/latest/) for generating lexical analysers.
  - [Happy](https://haskell-happy.readthedocs.io/en/latest/obtaining.html) for parsing generation.

## Executing
  Run the commands:
  - `cabal build`
  - `cabal run < tests/input.c0`

### Examples
  File saved as `input.c0` into `/tests` directory:
  ```
int factorial(int n) {
    if (n == 0) 
      return 1;
    return n * factorial(n-1);
}

int main() {
    print_int(factorial(scan_int()));
}
```

## Contributors
  - [carlahnr](https://github.com/carlahnr/)
  - Rui Santos
