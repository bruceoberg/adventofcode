#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)

; day 3

(define g_pathData
    (build-path
        g_pathCode
        "3.txt"))

(define g_lStr (file->lines g_pathData))

(define g_laCh (map (lambda (str) (list->vector (string->list str))) g_lStr))
(define g_aaCh (list->vector g_laCh))

(define g_xMax (vector-length (vector-ref g_aaCh 0)))
(define g_yMax (vector-length g_aaCh))

(displayln g_xMax)
(displayln g_yMax)

(define g_dx 3)
(define g_dy 1)

(define (CFromXy xy)
    (match-define (list dx dy) xy)
    (let go ([x dx] [y dy] [cT 0])
        (define xW (modulo x g_xMax))
        (define yW (modulo y g_yMax))
        (define ch (vector-ref (vector-ref g_aaCh yW) xW))
        (define xNext (+ x dx))
        (define yNext (+ y dy))
        (cond
            [(>= y g_yMax) cT]
            [(equal? ch #\.) (go xNext yNext cT)]
            [else (go xNext yNext (add1 cT))])))

(define g_lXy
    (list
        (list 1 1)
        (list 3 1)
        (list 5 1)
        (list 7 1)
        (list 1 2)
    )
)

(define g_lC (map CFromXy g_lXy))

g_lC

(apply * g_lC)