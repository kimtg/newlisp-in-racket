# newLISP in Racket

A high-fidelity newLISP runtime, interpreter, and optimizing transpiler written in [Racket](https://racket-lang.org).

This project implements the semantics of newLISP v10.7+—including dynamic scoping, context namespaces, Functional Object-Oriented Programming (FOOP), tree dictionaries, and implicit list/string indexing—while offering a high-performance transpilation engine compiling newLISP expressions directly into native Racket bytecode.

---

## Key Features

- **Optimizing Transpiler & JIT Compilation**:
  - Translates newLISP AST directly to native Racket syntax and bytecode (`nl-transpile.rkt`), featuring zero-overhead dynamic scoping, unboxed fast math, and cached inline method dispatch.
  - High-fidelity runtime environment and dynamic scope manager (`nl-eval.rkt`) governing contexts, symbols, place mutations, and runtime reflection.
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

## Command-Line Options

Display the built-in help text at any time with the `-h` flag:

```bash
racket main.rkt -h
# or with standalone executable:
./newlisp.exe -h
```

```text
 -h this help (no init.lsp)
 -n no init.lsp (must be first)
 -x <source> <target> link (no init.lsp)
 -v version
 -s <stacksize>
 -m <max-mem-MB> cell memory
 -e <quoted lisp expression>
 -l <path-file> log connections
 -L <path-file> log all
 -w <working dir>
 -c no prompts, HTTP
 -C force prompts
 -t <usec-server-timeout>
 -p <port-no>
 -d <port-no> demon mode
 -http only
 -http-safe safe mode
 -6 IPv6 mode
```

### Options Reference

| Option | Syntax / Arguments | Description |
| :--- | :--- | :--- |
| `-h` | `-h` | Display the command-line help text and exit (suppresses `init.lsp`). |
| `-n` | `-n` | Do not load `init.lsp` or `.init.lsp` (must be the first option on the command line). |
| `-x` | `-x <source> <target>` | Link and embed a source Lisp script into a target standalone binary without loading `init.lsp`. |
| `-v` | `-v` | Display version banner (`newLISP v.10.7.6 [Racket] ...`) and exit. |
| `-s` | `-s <stacksize>` | Set the maximum evaluation call stack depth (default: `1024`). Can be attached (e.g., `-s2048`). |
| `-m` | `-m <max-mem-MB>` | Set the maximum cell memory limit in megabytes. Can be attached (e.g., `-m64`). |
| `-e` | `-e "<expr>"` | Evaluate the quoted newLISP expression and print the result. Multiple `-e` flags can be chained. |
| `-l` | `-l <path-file>` | Log network and HTTP connections to the specified file. |
| `-L` | `-L <path-file>` | Log all incoming and outgoing network and HTTP traffic to the specified file. |
| `-w` | `-w <dir>` | Set the initial working directory for script execution. |
| `-c` | `-c` | Suppress interactive prompts and banner (ideal for HTTP server or piped batch mode). |
| `-C` | `-C` | Force prompt display even when input is redirected or after running scripts. |
| `-t` | `-t <usec>` | Set the server network socket timeout in microseconds. |
| `-p` | `-p <port-no>` | Start a TCP/HTTP server on `<port-no>` in single-session mode (exits after handling). |
| `-d` | `-d <port-no>` | Start a TCP/HTTP server in daemon mode (continuously listens and accepts connections). |
| `-http` | `-http` | Restrict server mode to HTTP requests only (rejects raw Lisp socket commands). |
| `-http-safe` | `-http-safe` | Restrict HTTP server to safe mode (blocks directory traversal attempts containing `..` or `//`). |
| `-6` | `-6` | Enable IPv6 networking mode for socket operations and servers. |

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
├── nl-eval.rkt                # Runtime environment, context registry & dynamic scope manager
├── nl-transpile.rkt           # Optimizing transpiler & compiler to native Racket bytecode
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
│   ├── test-all.rkt           # Unit test suite via runtime eval-body (96 tests)
│   ├── test-compiled.rkt      # Unit test suite via direct transpiler (96 tests)
│   └── test-cli.rkt           # CLI, option flags, and subprocess test suite (30 tests)
├── benchmarks/
│   └── bench-compare.rkt      # Benchmark against original newLISP (C binary vs Racket)
├── demo.lsp                   # Feature demonstration script
└── newlisp_manual.html        # Complete reference manual for newLISP 10.7.5
```

---

## Benchmark Against Original newLISP

The optimizing transpiler (`nl-transpile.rkt`) compiles newLISP ASTs into native Racket forms, utilizing unboxed operations, direct identifier bindings, optimized place mutations, and native Racket bytecode compilation to rival and often surpass the original C newLISP engine.

Run the benchmark suite:
```bash
racket benchmarks/bench-compare.rkt
```

### Benchmark Results (Original C newLISP vs Racket)

| Benchmark | Original C newLISP (v10.7.1) | Racket Transpiled | Transpiled vs Original newLISP |
| :--- | :--- | :--- | :--- |
| **Recursive Fibonacci** (`fib 30`) | `245.0 ms` | `180.0 ms` | **1.4x faster** |
| **Tight Loop Mutation** (`dotimes 1,000,000` with `++`) | `27.3 ms` | `6.6 ms` | **4.1x faster** |
| **List Operations** (`sequence`, `map`, `filter` 100k items) | `26.1 ms` | `20.3 ms` | **1.3x faster** |
| **FOOP Method Dispatch** (100,000 invocations) | `11.4 ms` | `19.8 ms` | `0.60x relative` |

---

## Verification & Testing

The test suite covers arithmetic, string handling, dynamic scoping, FOOP dispatch, place mutation, networking, HTTP requests, binary packing, matrices, and command-line interfaces.

```bash
# Run test suite via runtime eval-body (96 tests)
racket tests/test-all.rkt

# Run test suite via direct transpiler (96 tests)
racket tests/test-compiled.rkt

# Run CLI and flags test suite (30 tests)
racket tests/test-cli.rkt
```

All 222 tests pass across the entire test suite.

---

## License

This project is free and unencumbered software released into the public domain under [The Unlicense](LICENSE). See the [LICENSE](LICENSE) file for details.

