# cacco

Cacco is a programming language.

**Note:** Cacco is still in the design stage. It's not working yet.

## Table of Contents
- [Introduction](#introduction)
- [Development](#development)

---

## Introduction

### Comment
```
;; This is inline comment (to the end of the line).

;/ This is block comment.
  ;/ These block comments can be nested. /;
/;
```
### Literals
```
0     ;; Fuzzy Numeric.  (default: Natural)
-1 +1 ;; Fuzzy Integer.  (default: Integer)
0.0   ;; Fuzyy Decimal.  (default: Decimal)
1/2   ;; Fuzzy Rational. (default: (Ratio Integer))
'a'   ;; Charactor.      (same as Word32)
""    ;; Fuzzy Text.     (default: String_UTF8)
:key  ;; Keyword.
[]    ;; Fuzzy Sequence. (default: Linked_List)
{}    ;; Key-Value Map.
```

### Declaration and Reassign
```
;; Declare procedure or value or variable.
(dec x Integer)

;; Define a constant value once.
(let x 1000)

;; Declare a mutable variable.
(var y 0)

;; Strictly typing with literal.
(val z (as 0 Word8))
(var w (as "something" ByteString))

;; Reassign
(set! y 1000)    ;; OK
(set! y "hello") ;; Can't. `y` has been declered as Number type.
(set! x 10)      ;; Can't. `x` has been declered as immutable.
```

### Calling procedure
```
;; Simply calling a procedure.
(foo a b c)

;; Calling closure.
((fun [a] (* a a)) 10) ;; returns 100
```

### Define procedure
```
;; Declare a procedure
(dec make_linear
     ;; (let: (t Type) ...)
     ;; Generic typing form.
     (let: (n Number)
           ;; (fun: T1 T2 ... Tn)
           ;; Function type recieve T1, T2 ... then return value as `Tn` type.
           (fun: n n (fun: n n))))

;; Define the procedure
(let make_linear
     (fun [a b]
          (fun [x] (+ (* a x) b))))
```

---

## Development

### Prerequisites

- Haskell Stack

### Build and Testing
#### Simply building libraries and application
```sh
stack build
```

#### Clean
```sh
stack clean
```

#### Run Tests
```sh
# simply run test-suites
stack test

# run test-suites and generate coverage-report
stack clean && stack test --coverage
# open coverage-report in your browser
open $(stack path --local-hpc-root)/index.html
```
