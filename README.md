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
oldlisp/
├── main.rkt                   # CLI driver & entry point (-e, REPL, script execution)
├── nl-types.rkt               # Core data types, symbols, contexts, and environment
├── nl-reader.rkt              # Tokenizer and reader (strings, numbers, symbols, lists)
├── nl-eval.rkt                # Tree-walking interpreter & dynamic scope manager
├── nl-transpile.rkt           # Optimizing transpiler to native Racket syntax
├── nl-builtins.rkt            # Core built-in primitives and control flow
├── nl-ext.rkt                 # Extended utility functions
├── nl-macros.rkt              # Built-in and user-defined macros
├── nl-files.rkt               # File system primitives
├── nl-net.rkt                 # TCP socket networking
├── nl-math-ext.rkt            # Linear algebra, statistical, and financial functions
├── nl-http.rkt                # HTTP client (get-url)
├── nl-repl.rkt                # Multi-line interactive REPL
├── newlisp/
│   └── lang/
│       └── reader.rkt         # `#lang newlisp` reader module for Racket integration
├── tests/
│   ├── test-all.rkt           # Unit test suite for interpreter (90 tests)
│   └── test-compiled.rkt      # Unit test suite for transpiler (90 tests)
├── benchmarks/
│   └── bench-compare.rkt      # Benchmark comparing interpreted vs transpiled performance
├── demo.lsp                   # Feature demonstration script
└── newlisp_manual.html        # Complete reference manual for newLISP 10.7.5
```

---

## Performance & Benchmarks

The transpiler (`nl-transpile.rkt`) compiles newLISP ASTs into Racket constructs, utilizing unboxed operations, direct identifier bindings, and place mutations where possible.

Run the benchmark suite:
```bash
racket benchmarks/bench-compare.rkt
```

### Sample Benchmark Results

| Benchmark | Interpreted | Transpiled | Speedup |
| :--- | :--- | :--- | :--- |
| **Recursive Fibonacci** (`fib 30`) | `2926 ms` | `141 ms` | **20.7x faster** |
| **Tight Loop Mutation** (`dotimes 1,000,000` with `++`) | `847 ms` | `6.9 ms` | **122.3x faster** |
| **List Operations** (`sequence`, `map`, `filter` 100,000 items) | `143 ms` | `149 ms` | **~1.0x** |
| **FOOP Method Dispatch** (100,000 invocations) | `462 ms` | `476 ms` | **~1.0x** |

---

## Verification & Testing

The test suite covers arithmetic, string handling, dynamic scoping, FOOP dispatch, place mutation, networking, HTTP requests, binary packing, and matrices.

```bash
# Run interpreter test suite
racket tests/test-all.rkt

# Run transpiled / compiled test suite
racket tests/test-compiled.rkt
```

Both test suites validate 90 individual test assertions across the entire feature set.

---

## License

This project is free and unencumbered software released into the public domain under [The Unlicense](LICENSE). See the [LICENSE](LICENSE) file for details.

