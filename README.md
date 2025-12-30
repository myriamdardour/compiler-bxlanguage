# BX Compiler

This is my implementation of a compiler for the BX language, written in Python. It compiles source code into x64 (Linux) or ARM64 (macOS) assembly.

## Project Structure

* **`bxc.py`**: The main driver script. It handles command-line arguments, runs the pipeline, and calls GCC for linking.
* **`bxlexer.py` / `bxparser.py`**: Lexer and Parser implementation using `ply`. Generates the AST defined in `bxast.py`.
* **`bxtychecker.py`**: Performs semantic analysis, type checking, and scope resolution.
* **`bxmm.py`**: Converts the AST to Three-Address Code (TAC) and handles "Maximal Munch". This is where closure conversion and name mangling happen.
* **`bxasmgen.py`**: Lowers TAC to assembly instructions. Contains an abstract base class with implementations for x64 and ARM64.
* **`bxruntime.c`**: C runtime library for `print` functions.

## Design Choices

### 1. Nested Functions & Closures
To support functions as first-class citizens and nested procedure declarations, I implemented **closures** (fat pointers). 

* **Fat Pointers:** When a function is used as a value (e.g., passed as an argument), it is represented as a 16-byte structure on the stack:
    1.  The code pointer (address of the function label).
    2.  The static link (pointer to the enclosing stack frame).
* **Static Links:** The backend passes the static link as a hidden first argument. When accessing a variable from an outer scope, the compiler walks up the chain of static links to find the correct frame.
* **Global Functions:** Global functions are also wrapped in closures when passed as arguments, but their static link is set to 0 (null).

### 2. Name Mangling
In high-level languages like BX, you can have:

Nested functions: A function g inside main is different from a global function g.

Shadowing: You can define a function g inside a block, and then another g inside a different block within the same function.

In Assembly, however, every label (function name) must be globally unique. If you just called them all g, the assembler wouldn't know which one you meant.
To handle variable shadowing and nested functions with the same name, I implemented a unique naming scheme in `bxmm.py`.
* Functions are mangled as `outer__inner__nestDepth.uid`.
* The `uid` is a global counter that ensures that even if two functions have the exact same name and nesting depth (e.g., inside different blocks), they get unique assembly labels.

### 3. Type Checking & Safety
The type checker (`bxtychecker.py`) runs in two passes:
1.  **Pre-typing:** Scans global variables and function signatures to build the initial scope.
2.  **Checking:** Validates the function bodies.

I added static checks for specific runtime errors involving constants:
* Division or modulus by zero (e.g., `x / 0`).
* Bitwise shifts by a negative amount (e.g., `x << -1`).

### 4. Extern Declarations 
I was unsure about this, but this is how I decided to proceed, to make it work with my tests. The compiler strictly requires `extern` declarations for foreign functions. The `print` function is treated as a special case in the parser/type-checker to map to the runtime's `print_int` or `print_bool` depending on the argument type.

### 5. Multi-Architecture Support
The backend logic in `bxasmgen.py` is decoupled from the specific assembly syntax.
* `AsmGen` is a base class that handles TAC traversal and temporary variable management.
* `AsmGen_x64_Linux` handles x86-64 specific opcodes and registers (`%rdi`, `%rax`, etc.).
* `AsmGen_arm64_Darwin` handles ARM64 opcodes and registers (`X0`, `FP`, `LR`) for macOS.
This way was longer to figure out but easier overall, since I was testing using my own laptop (mac) and the ssh into a lab computer was not working.

## How to Run 

To compile a `.bx` file:

```bash
python3.12 tests/program.bx
python3.12 gcc tests/program.s bxlib/bxruntime.c -o program
python3.12 ./program
