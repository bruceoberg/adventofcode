#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)

; (require racket/runtime-path)
; (define-runtime-path pathData "1.txt")
(define g_pathData
    (build-path
        g_pathCode
        "10.txt"))

(define g_lStr (file->lines g_pathData))
(define g_lN (sort (map string->number g_lStr) <))

(define g_lNAll (append '(0) g_lN (list (+ 3 (last g_lN)))))

(define g_lNLess (take g_lNAll (sub1 (length g_lNAll))))
(define g_lNMore (rest g_lNAll))

;g_lNAll

(define g_lNDiff
    (for/list ([nLess g_lNLess] [nMore g_lNMore])
        (- nMore nLess)
    )
)

;g_lNDiff

(define c1 (length (filter (lambda (n) (equal? n 1)) g_lNDiff)))
(define c2 (length (filter (lambda (n) (equal? n 2)) g_lNDiff)))
(define c3 (length (filter (lambda (n) (equal? n 3)) g_lNDiff)))

(displayln "ones:")
c1

(displayln "twos:")
c2

(displayln "threes:")
c3

(displayln "product:")
(* c1 c3)
