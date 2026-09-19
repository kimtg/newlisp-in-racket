#lang racket/base

(require racket/file
         racket/string
         racket/math
         "../nl-types.rkt"
         "../nl-reader.rkt"
         "../nl-eval.rkt"
         "../nl-builtins.rkt")

(define total-tests 0)
(define passed-tests 0)
(define failed-tests 0)

(define (eval-nl code-str)
  (define exprs
    (nl-read-all code-str (lambda (s) (find-or-create-symbol s (current-context)))))
  (eval-body exprs))

(define (check-nl desc code-str expected-str)
  (set! total-tests (+ total-tests 1))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (set! failed-tests (+ failed-tests 1))
                     (printf "[FAIL] ~a\n  Code: ~a\n  Error: ~a\n" desc code-str (exn-message e)))])
    (define res (eval-nl code-str))
    (define actual-str (nl->string res #t))
    (if (string=? actual-str expected-str)
        (begin
          (set! passed-tests (+ passed-tests 1))
          (printf "[PASS] ~a\n" desc))
        (begin
          (set! failed-tests (+ failed-tests 1))
          (printf "[FAIL] ~a\n  Code:     ~a\n  Expected: ~a\n  Actual:   ~a\n"
                  desc code-str expected-str actual-str)))))

(printf "====================================================\n")
(printf "        newLISP in Racket Test Suite                \n")
(printf "====================================================\n\n")

;; 1. Literal and Numbers
(check-nl "Integer addition" "(+ 1 2 3 4)" "10")
(check-nl "Hexadecimal literal" "0xE8" "232")
(check-nl "Binary literal" "0b101010" "42")
(check-nl "Octal literal" "055" "45")
(check-nl "Big integer literal" "12345678901234567890L" "12345678901234567890")
(check-nl "Float literal" "1.25" "1.25")

;; 2. String literal syntax
(check-nl "Quoted string with escapes" "\"hello\\nworld\"" "\"hello\\nworld\"")
(check-nl "Decimal ASCII escape in string" "\"\\065\\066\\067\"" "\"ABC\"")
(check-nl "Hex ASCII escape in string" "\"\\x41\\x42\\x43\"" "\"ABC\"")
(check-nl "Curly brace string" "{this \"is\" a string}" "\"this \\\"is\\\" a string\"")
(check-nl "Raw text block string" "[text]raw text block[/text]" "\"raw text block\"")

;; 3. nil, true, and ()
(check-nl "nil evaluates to nil" "nil" "nil")
(check-nl "true evaluates to true" "true" "true")
(check-nl "empty list evaluates to empty list" "()" "()")
(check-nl "nil is atom" "(atom? nil)" "true")
(check-nl "() is not atom" "(atom? '())" "nil")
(check-nl "() is empty" "(empty? '())" "true")
(check-nl "nil is nil?" "(nil? nil)" "true")
(check-nl "() is not nil?" "(nil? '())" "nil")

;; 4. Arithmetic & Math
(check-nl "Integer arithmetic +, -, *, /" "(/ (* (+ 10 20) 2) (- 10 5))" "12")
(check-nl "Modulo %" "(% 17 5)" "2")
(check-nl "Math abs, min, max" "(list (abs -42) (min 5 3 8) (max 5 3 8))" "(42 3 8)")
(check-nl "Math sqrt and pow" "(list (sqrt 16) (pow 2 8))" "(4 256)")
(check-nl "Math round, floor, ceil" "(list (floor 3.7) (ceil 3.2) (round 3.6))" "(3.0 4.0 4)")

;; 5. Bitwise
(check-nl "Bitwise and/or/xor/shift" "(list (& 0xF0 0x0F) (| 0xF0 0x0F) (^ 0xFF 0x0F) (<< 1 4))" "(0 255 240 16)")

;; 6. Dynamic Scoping
(check-nl "Dynamic scoping variable resolution"
          "(begin (set 'x 1) (define (f) x) (define (g x) (f)) (list (f) (g 0) (f)))"
          "(1 0 1)")

;; 7. Lambda expressions and parameter features
(check-nl "Lambda treated as list (first)" "(first (lambda (x) (+ x x)))" "(x)")
(check-nl "Lambda treated as list (last)" "(last (lambda (x) (+ x x)))" "(+ x x)")
(check-nl "Optional parameters with default value"
          "(begin (define (greet name (greeting \"Hello\")) (string greeting \" \" name)) (list (greet \"Alice\") (greet \"Bob\" \"Hi\")))"
          "(\"Hello Alice\" \"Hi Bob\")")
(check-nl "Commas in parameter lists"
          "(begin (define (f a b , x y) (set 'x (+ a b)) (set 'y (* a b)) (list x y)) (f 3 4))"
          "(7 12)")
(check-nl "Unbound args via (args)"
          "(begin (define (f a) (args)) (f 1 2 3 4))"
          "(2 3 4)")

;; 8. Contexts & Namespaces
(check-nl "Symbols in separate contexts"
          "(begin (set 'FOO:val 100) (set 'BAR:val 200) (list FOO:val BAR:val))"
          "(100 200)")
(check-nl "Context default functor as function with memory"
          "(begin (define (Gen:Gen x) (if Gen:acc (inc Gen:acc x) (setq Gen:acc x))) (list (Gen 1) (Gen 2) (Gen 3)))"
          "(1 3.0 6.0)")
(check-nl "Tree context dictionary lookup and store"
          "(begin (define MyTree:MyTree) (MyTree \"apple\" 5) (MyTree \"banana\" 10) (list (MyTree \"apple\") (MyTree \"banana\") (MyTree \"cherry\")))"
          "(5 10 nil)")

;; 9. Functional Object-Oriented Programming (FOOP)
(check-nl "FOOP constructor and method dispatch"
          "(begin (new Class 'Circle) (define (Circle:area) (mul (pow (self 3) 2) 3.14159265)) (set 'c (Circle 0 0 10)) (:area c))"
          "314.159265")
(check-nl "FOOP mutable object method"
          "(begin (new Class 'Point) (define (Point:move dx dy) (inc (self 1) dx) (inc (self 2) dy)) (set 'p (Point 10 20)) (:move p 5 5) p)"
          "(Point 15.0 25.0)")

;; 10. Implicit Indexing & Slicing
(check-nl "Implicit list indexing"
          "(begin (set 'lst '(a b c d e)) (list (lst 0) (lst 2) (lst -1)))"
          "(a c e)")
(check-nl "Implicit rest (1 lst)"
          "(begin (set 'lst '(a b c d e)) (1 lst))"
          "(b c d e)")
(check-nl "Implicit slice (2 2 lst)"
          "(begin (set 'lst '(a b c d e)) (2 2 lst))"
          "(c d)")
(check-nl "Implicit string slice (2 3 \"abcdefg\")"
          "(2 3 \"abcdefg\")"
          "\"cde\"")

;; 11. Place Mutation (setq / setf / inc / dec / ++ / -- / push / pop / swap)
(check-nl "setf on list index"
          "(begin (set 'lst '(10 20 30)) (setf (lst 1) 99) lst)"
          "(10 99 30)")
(check-nl "setf on string index"
          "(begin (set 's \"hello\") (setf (s 0) \"H\") s)"
          "\"Hello\"")
(check-nl "++ and -- place mutations"
          "(begin (set 'v 10) (++ v 5) (-- v 2) v)"
          "13")
(check-nl "push and pop on list"
          "(begin (set 'lst '(2 3)) (push 1 lst) (define popped (pop lst)) (list popped lst))"
          "(1 (2 3))")
(check-nl "swap places"
          "(begin (setq x 1 y 2) (swap x y) (list x y))"
          "(2 1)")

;; 12. Control Flow
(check-nl "Multi-branch if with condition fallback"
          "(begin (define (classify n) (if (< n 0) \"neg\" (< n 10) \"small\" \"big\")) (list (classify -5) (classify 5) (classify 50)))"
          "(\"neg\" \"small\" \"big\")")
(check-nl "if sets $it"
          "(begin (set 'lst '(1 2 3)) (if lst (last $it)))"
          "3")
(check-nl "cond"
          "(cond ((= 1 2) \"no\") ((= 2 2) \"yes\") (true \"default\"))"
          "\"yes\"")
(check-nl "case"
          "(case 2 (1 \"one\") (2 \"two\") (3 \"three\"))"
          "\"two\"")
(check-nl "dotimes with early break condition"
          "(begin (set 'res '()) (dotimes (i 10 (= i 5)) (push i res -1)) res)"
          "(0 1 2 3 4)")
(check-nl "dolist loop"
          "(begin (set 'total 0) (dolist (x '(1 2 3 4)) (++ total x)) total)"
          "10")
(check-nl "while loop with $idx"
          "(begin (set 'x 0) (while (< x 3) (++ x)) $idx)"
          "2")
(check-nl "catch and throw"
          "(catch (dotimes (i 100) (if (= i 42) (throw i))))"
          "42")
(check-nl "catch error capture"
          "(begin (list (catch (/ 1 0) 'err) (string? err)))"
          "(nil true)")

;; 13. List processing built-ins
(check-nl "map with lambda"
          "(map (lambda (x) (* x 2)) '(1 2 3))"
          "(2 4 6)")
(check-nl "filter"
          "(filter odd? '(1 2 3 4 5 6))"
          "(1 3 5)")
(check-nl "clean"
          "(clean odd? '(1 2 3 4 5 6))"
          "(2 4 6)")
(check-nl "flat"
          "(flat '((1 2) (3 (4 5))))"
          "(1 2 3 4 5)")
(check-nl "difference and intersect"
          "(list (difference '(1 2 3 4) '(2 4)) (intersect '(1 2 3 4) '(2 4 5)))"
          "((1 3) (2 4))")
(check-nl "unique"
          "(unique '(a b a c b d))"
          "(a b c d)")
(check-nl "sequence and dup"
          "(list (sequence 1 5) (dup \"ha\" 3))"
          "((1 2 3 4 5) \"hahaha\")")
(check-nl "assoc and lookup"
          "(begin (set 'al '((name \"Alice\") (age 30))) (list (assoc 'name al) (lookup 'age al)))"
          "((name \"Alice\") 30)")

;; 14. String & Regex
(check-nl "format printf"
          "(format \"Name: %s, Age: %d, Hex: 0x%X\" \"Alice\" 30 255)"
          "\"Name: Alice, Age: 30, Hex: 0xFF\"")
(check-nl "regex with captures"
          "(regex \"([a-z]+)-([0-9]+)\" \"item-123\")"
          "(\"item-123\" \"item\" \"123\")")
(check-nl "replace in string"
          "(replace \"world\" \"hello world!\" \"newLISP\")"
          "\"hello newLISP!\"")
(check-nl "starts-with and ends-with"
          "(list (starts-with \"filename.lsp\" \"file\") (ends-with \"filename.lsp\" \".lsp\"))"
          "(true true)")

;; 15. Arrays & Matrix
(check-nl "array creation and array-list"
          "(array-list (array 2 2 '(1 2 3 4)))"
          "((1 2) (3 4))")
(check-nl "transpose matrix"
          "(transpose '((1 2) (3 4)))"
          "((1 3) (2 4))")

;; 16. File I/O
(check-nl "write-file and read-file"
          "(begin (write-file \"temp_test.txt\" \"Hello newLISP!\") (define content (read-file \"temp_test.txt\")) (delete-file \"temp_test.txt\") content)"
          "\"Hello newLISP!\"")

;; 17. HTTP get-url & Networking
(check-nl "get-url header retrieval"
          "(begin (define h (get-url \"http://example.com\" \"header\")) (string? h))"
          "true")

(check-nl "get-url file:// retrieval"
          "(begin (write-file \"temp_url.txt\" \"Local URL test\") (define c (get-url \"file://temp_url.txt\")) (delete-file \"temp_url.txt\") c)"
          "\"Local URL test\"")

(check-nl "get-url integer timeout parameter"
          "(string? (get-url \"http://example.com\" 5000))"
          "true")

(check-nl "get-url list option with status"
          "(begin (define r (get-url \"http://example.com\" \"list\")) (and (list? r) (= (length r) 3) (= (nth 2 r) 200)))"
          "true")

(check-nl "get-url nonexistent file error"
          "(starts-with (get-url \"file://nonexistent_file_xyz.txt\") \"ERR:\")"
          "true")

(check-nl "base64-enc and base64-dec"
          "(base64-dec (base64-enc \"Hello newLISP Web!\"))"
          "\"Hello newLISP Web!\"")

(check-nl "crc32 checksum"
          "(= (crc32 \"123456789\") 3421780262)"
          "true")

(check-nl "pack and unpack binary"
          "(unpack \">d\" (pack \">d\" 123456))"
          "(123456)")

;; 18. New Special Forms
(check-nl "bind association list"
          "(begin (bind '((b1 11) (b2 22))) (+ b1 b2))"
          "33")

(check-nl "constant protection"
          "(begin (constant cval 99) cval)"
          "99")

(check-nl "collect until nil"
          "(begin (setq c-cnt 0) (collect (if (< c-cnt 3) (++ c-cnt) nil)))"
          "(1 2 3)")

(check-nl "local dynamic scoping"
          "(begin (setq loc-x 10) (define res (local (loc-x) (setq loc-x 99) loc-x)) (list res loc-x))"
          "(99 10)")

;; 19. Advanced Math & Matrix
(check-nl "matrix determinant"
          "(det '((1 2) (3 4)))"
          "-2.0")

(check-nl "matrix multiply and invert"
          "(begin (define m '((4 7) (2 6))) (define inv (invert m)) (define prod (multiply m inv)) (round (prod 0 0)))"
          "1")

(check-nl "prime factor"
          "(factor 60)"
          "(2 2 3 5)")

(check-nl "financial fv calculation"
          "(round (fv 0.05 10 -100))"
          "1258")

;; 20. Pattern Matching & List Ops
(check-nl "wildcard match"
          "(match '(a ? b * d) '(a 1 b 2 3 d))"
          "(1 (2 3))")

(check-nl "index and select"
          "(select '(a b c d e) (index (fn (x) (or (= x 'b) (= x 'd))) '(a b c d e)))"
          "(b d)")

;; 21. Streams & System
(check-nl "pipe IPC stream read and write"
          "(begin (define p (pipe)) (write-line (p 1) \"pipe message\") (define msg (read-line (p 0))) (close (p 0)) (close (p 1)) msg)"
          "\"pipe message\"")

(check-nl "date-list representation"
          "(begin (define dl (date-list)) (and (list? dl) (= (length dl) 10)))"
          "true")

(check-nl "core predicates null?, quote?, legal?"
          "(and (null? nil) (null? '()) (quote? '(quote (1 2))) (legal? \"my-var\"))"
          "true")

(printf "\n====================================================\n")
(printf "Tests Total:  ~a\n" total-tests)
(printf "Tests Passed: ~a\n" passed-tests)
(printf "Tests Failed: ~a\n" failed-tests)
(printf "====================================================\n")

(if (= failed-tests 0)
    (exit 0)
    (exit 1))
