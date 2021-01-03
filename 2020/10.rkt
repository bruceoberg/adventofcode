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

(define c1 (length (filter (lambda (n) (equal? n 1)) g_lNDiff)))
(define c2 (length (filter (lambda (n) (equal? n 2)) g_lNDiff)))
(define c3 (length (filter (lambda (n) (equal? n 3)) g_lNDiff)))

; (displayln "ones:")
; c1

; (displayln "twos:")
; c2

; (displayln "threes:")
; c3

; (displayln "product:")
; (* c1 c3)

(define g_lC1
    (let go ([lNDiff g_lNDiff] [c1 0] [lC1 '()])
        (cond
            ([empty? lNDiff] (reverse (cons c1 lC1)))
            (else
                (define nDiff (first lNDiff))
                (define lNDiffGo (rest lNDiff))
                (if (equal? nDiff 1)
                    (go lNDiffGo (add1 c1) lC1)
                    (go lNDiffGo 0 (cons c1 lC1))
                )
            )
        )
    )
)

g_lNDiff
g_lC1
(apply max g_lC1)

(define g_c1_2 (length (filter (lambda (c1) (equal? c1 2)) g_lC1)))
(define g_c1_3 (length (filter (lambda (c1) (equal? c1 3)) g_lC1)))
(define g_c1_4 (length (filter (lambda (c1) (equal? c1 4)) g_lC1)))
g_c1_2
g_c1_3
g_c1_4

(define g_p2 (expt 2 g_c1_2))
(define g_p4 (expt 4 g_c1_3))
(define g_p7 (expt 7 g_c1_4))

(define g_pow (* g_p2 g_p4 g_p7))
g_pow
