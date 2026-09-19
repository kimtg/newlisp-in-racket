# newLISP in Racket

A high-fidelity newLISP runtime, interpreter, and optimizing transpiler written in [Racket](https://racket-lang.org).

This project implements the semantics of newLISP v10.7+—including dynamic scoping, context namespaces, Functional Object-Oriented Programming (FOOP), tree dictionaries, and implicit list/string indexing—while offering a high-performance transpilation engine compiling newLISP expressions directly into native Racket bytecode.

---

## Key Features

- **Dual Execution Modes**:
  - **Dynamic Tree-Walking Interpreter** (`nl-eval.rkt`): Faithfully replicates newLISP dynamic scoping, environments, and macro expansion.
  - **Optimizing Transpiler** (`nl-transpile.rkt`): Translates newLISP AST to optimized Racket forms, achieving **20x to 120x speedups** on iterative and recursive code.
- **Racket `#lang` Integration**:
  - Native `#lang` reader (`newlisp/lang/reader.rkt`) enables writing standalone newLISP source files directly runnable in the Racket ecosystem.
- **Full newLISP Semantics**:
  - **Dynamic Scoping**: Runtime variable resolution with `local` contexts and stack-based binding.
  - **Namespaces & Contexts**: First-class context objects (`MAIN`, `Class`, user contexts) with prefix syntax (`Context:symbol`).
  - **FOOP (Functional Object-Oriented Programming)**: Struct/list-based objects, constructor generation, and method dispatch via `(:method object [args...])`.
  - **Default Functors & Tree Dictionaries**: Contexts callable as associative lookups and mutating stores: `(PhoneBook "Alice" "555-0101")`.
  - **Implicit Slicing & Indexing**: Indexing expressions like `(lst 0)`, `(lst -1)`, slices like `(start count lst)`, and string slices.
  - **Place Mutation**: In-place mutation primitives including `++`, `--`, `swap`, `push`, `pop`, and `setf` on collections and strings.
  - **Comprehensive Control Flow**: `if`, `if-not`, `cond`, `case`, `while`, `until`, `do-while`, `do-until`, `dotimes`, `dolist`, `dostring`, `for`, `catch`, and `throw`.
- **Extensive Standard Library**:
  - **File I/O**: `read-file`, `write-file`, `append-file`, `copy-file`, `delete-file`, `directory`, `change-dir`.
  - **HTTP Client**: `get-url` supporting query headers, status codes, timeouts, and `file://` URIs.
  - **Network Sockets**: TCP client and server operations via `net-listen`, `net-connect`, `net-receive`, `net-send`, and `net-close`.
  - **Mathematics & Linear Algebra**: Matrix determinant, matrix inversion, matrix multiplication, `prime-factors`, financial `fv`, and statistics.
  - **Data Utilities**: `pack`/`unpack` binary structures, `base64-enc`/`base64-dec`, `crc32`, and regex pattern matching.
- **Interactive REPL**:
  - Command-line interface with multi-line expression buffering, syntax error capture, and shell escape support (`!command`).

---

## Quick Start

### Requirements
- [Racket](https://download.racket-lang.org/) v8.0 or newer installed and available in your `PATH`.

### Running the Interactive REPL
Launch the interactive shell:
```bash
racket main.rkt
```
```text
newLISP v.10.7.6 [Racket] on Windows
> (+ 1 2 3 4)
10
> (define (double x) (* x 2))
(lambda (x) (* x 2))
> (map double '(10 20 30))
(20 40 60)
> (exit)
```

### One-Liner Evaluation (`-e`)
Evaluate a newLISP expression and print the result:
```bash
racket main.rkt -e "(map (fn (x) (* x x)) (sequence 1 5))"
# Output: (1 4 9 16 25)
```

### Running Scripts
Execute any `.lsp` script:
```bash
racket main.rkt demo.lsp
```

### Command-line Arguments
Scripts can access command-line arguments using `(main-args)` and `$args`:
```bash
racket main.rkt demo.lsp foo bar
```

### Running with `#lang newlisp`
You can write standalone source files with `#lang newlisp` (or `#lang reader "newlisp/lang/reader.rkt"`) and execute them directly with Racket:
```lisp
#lang newlisp
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(println "Fib 20: " (fib 20))
(println "Args: " (main-args))
```
```bash
racket -S . my_script.lsp foo bar
```

### Creating a Standalone Executable
You can compile the interpreter and runtime into a standalone native binary with all required dependencies embedded using `raco exe --embed-dlls`:

```bash
# Compile standalone executable embedding runtime DLLs (Windows)
raco exe --embed-dlls -o newlisp.exe main.rkt
```

The resulting `newlisp.exe` is self-contained and runs without needing Racket installed:

```bash
# Launch the interactive REPL
./newlisp.exe

# Evaluate an expression directly
./newlisp.exe -e "(println (+ 1 2 3))"

# Run a script with arguments
./newlisp.exe demo.lsp foo bar
```

---

## Code Examples

### 1. Functional Object-Oriented Programming (FOOP)
```lisp
(new Class 'Shape)
(define (Shape:area) 0)

(new Class 'Rectangle)
(define (Rectangle:area)
  (* (self 1) (self 2)))

(define (Rectangle:describe)
  (format "Rectangle [%dx%d], area = %d" (self 1) (self 2) (:area (self))))

(set 'rect (Rectangle 10 20))
(println (:describe rect))
;; Output: Rectangle [10x20], area = 200
```

### 2. Context Default Functors & Tree Dictionaries
```lisp
(define PhoneBook:PhoneBook)
(PhoneBook "Alice" "555-0101")
(PhoneBook "Bob"   "555-0102")

(println (PhoneBook "Alice"))
;; Output: 555-0101
```

### 3. Implicit Slicing and Indexing
```lisp
(set 'nums (sequence 1 10))

;; Slice (start count list):
(println (2 5 nums))
;; Output: (3 4 5 6 7)

;; Direct indexing:
(println (nums 0))   ; 1
(println (nums -1))  ; 10
```

### 4. Place Mutation
```lisp
(setq count 10)
(++ count 5)
(println count) ; 15

(setq fruits '("apple" "banana" "cherry"))
(setf (fruits 1) "blueberry")
(println fruits) ; ("apple" "blueberry" "cherry")
```

---

## Architecture & Codebase Layout

```
newlisp-in-racket/
├── main.rkt                   # CLI driver & entry point (-e, REPL, script execution)
├── nl-types.rkt               # Core data types, symbols, contexts, and environment
├── nl-reader.rkt              # Tokenizer and reader (strings, numbers, symbols, lists)
├── nl-eval.rkt                # Tree-walking interpreter & dynamic scope manager
├── nl-transpile.rkt           # Optimizing transpiler to native Racket syntax
├── nl-builtins.rkt            # Core built-in primitives and control flow
├── nl-ext.rkt                 # Extended utility functions
├── nl-macros.rkt              # Built-in macros & syntax transformers
├── nl-files.rkt               # File system primitives
├── nl-net.rkt                 # TCP socket networking
├── nl-math-ext.rkt            # Linear algebra, statistical, and financial functions
├── nl-http.rkt                # HTTP client (get-url)
├── nl-repl.rkt                # Multi-line interactive REPL
├── newlisp/
│   ├── main.rkt               # Package re-exports for collection usage
│   └── lang/
│       └── reader.rkt         # `#lang newlisp` reader module for Racket integration
├── tests/
│   ├── test-all.rkt           # Unit test suite for interpreter (96 tests)
│   ├── test-compiled.rkt      # Unit test suite for transpiler (96 tests)
│   └── test-cli.rkt           # CLI, option flags, and subprocess test suite (30 tests)
├── benchmarks/
│   └── bench-compare.rkt      # Benchmark comparing interpreted vs transpiled performance
├── demo.lsp                   # Feature demonstration script
└── newlisp_manual.html        # Complete reference manual for newLISP 10.7.5
```

---

## Performance & Benchmarks

The transpiler (`nl-transpile.rkt`) compiles newLISP ASTs into Racket constructs, utilizing unboxed operations, direct identifier bindings, optimized place mutations, and native Racket higher-order dispatch.

Run the benchmark suite:
```bash
racket benchmarks/bench-compare.rkt
```

### Sample Benchmark Results

| Benchmark | Interpreted | Transpiled | Speedup |
| :--- | :--- | :--- | :--- |
| **Recursive Fibonacci** (`fib 30`) | `2854 ms` | `137 ms` | **20.8x faster** |
| **Tight Loop Mutation** (`dotimes 1,000,000` with `++`) | `901 ms` | `6.5 ms` | **139.6x faster** |
| **List Operations** (`sequence`, `map`, `filter` 100,000 items) | `142 ms` | `17.3 ms` | **8.2x faster** |
| **FOOP Method Dispatch** (100,000 invocations) | `425 ms` | `77.6 ms` | **5.5x faster** |

---

## Verification & Testing

The test suite covers arithmetic, string handling, dynamic scoping, FOOP dispatch, place mutation, networking, HTTP requests, binary packing, matrices, and command-line interfaces.

```bash
# Run interpreter test suite (96 tests)
racket tests/test-all.rkt

# Run transpiled / compiled test suite (96 tests)
racket tests/test-compiled.rkt

# Run CLI and flags test suite (30 tests)
racket tests/test-cli.rkt
```

All 222 tests pass across the entire test suite.

---

## License

This project is free and unencumbered software released into the public domain under [The Unlicense](LICENSE). See the [LICENSE](LICENSE) file for details.

