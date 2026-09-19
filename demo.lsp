;;; demo.lsp - Demonstration of newLISP implemented in Racket

(println "--- 1. Testing Command-line Arguments ---")
(println "main-args: " (main-args))
(println "$args:     " $args)

(println "\n--- 2. Functional Object-Oriented Programming (FOOP) ---")
(new Class 'Shape)
(define (Shape:area) 0)

(new Class 'Rectangle)
;; Rectangle constructor sets width and height
(define (Rectangle:area)
  (* (self 1) (self 2)))

(define (Rectangle:describe)
  (format "Rectangle [%dx%d], area = %d" (self 1) (self 2) (:area (self))))

(set 'rect (Rectangle 10 20))
(println (:describe rect))

(println "\n--- 3. Context Default Functors & Tree Dictionaries ---")
;; Tree dictionary
(define PhoneBook:PhoneBook)
(PhoneBook "Alice" "555-0101")
(PhoneBook "Bob"   "555-0102")
(println "Alice's number: " (PhoneBook "Alice"))
(println "Bob's number:   " (PhoneBook "Bob"))

(println "\n--- 4. HTTP get-url ---")
(define body (get-url "https://httpbin.org/base64/SFRUUCBnZXQtdXJsIHdvcmtzIQ=="))
(println "Body received: " (trim body))

(println "\n--- 5. Implicit Slicing and Indexing ---")
(set 'numbers (sequence 1 10))
(println "Original: " numbers)
(println "Slice (2 5 numbers): " (2 5 numbers))
(println "Index 0: " (numbers 0) ", Index -1: " (numbers -1))

(println "\n[SUCCESS] newLISP script completed successfully!")
