#lang racket

(require mzlib/etc)
(define pathCode (this-expression-source-directory))

(require srfi/1) ; drop-right

(require racket/trace)

; (require racket/runtime-path)
; (define-runtime-path pathData "1.txt")
(define pathData
    (build-path
        pathCode
        "9.txt"))

(define lStrFile (file->lines pathData))
(define lNFile (map string->number lStrFile))
(define cnWin (first lNFile))
(define lN (rest lNFile))
(define-values (lNCur lNRemain) (split-at lN cnWin))
(define vNCur (list->vector lNCur))

(define lIjAll (for*/list ([i (range cnWin)] [j (range i)]) (cons i j)))
(define lIij (for*/list ([i (range cnWin)]) (cons i (filter (lambda (ij) (or (equal? i (car ij)) (equal? i (cdr ij)))) lIjAll))))
(define mpILIj (make-hash lIij))

(define (SumFromIj ij) (+ (vector-ref vNCur (car ij)) (vector-ref vNCur (cdr ij))))

(define mpSumSetIj
    (let go ([mpSumSetIj (make-hash)] [lIj lIjAll])
        (cond
            ([empty? lIj] mpSumSetIj)
            (else
                (begin
                    (define ij (first lIj))
                    (define lIjNext (rest lIj))
                    (define sum (SumFromIj ij))
                    (define setIj (hash-ref! mpSumSetIj sum (mutable-set)))
                    (set-add! setIj ij)

                    (go mpSumSetIj lIjNext)
                )
            )
        )
    )
)

(define (UpdateMp iN n)
    (define lIj (hash-ref mpILIj iN))
    (for ([ij lIj])
        (define sum (SumFromIj ij))
        (define setIj (hash-ref! mpSumSetIj sum (mutable-set)))
        (set-remove! setIj ij)
    )
    (vector-set! vNCur iN n)
    (for ([ij lIj])
        (define sum (SumFromIj ij))
        (define setIj (hash-ref! mpSumSetIj sum (mutable-set)))
        (set-add! setIj ij)
    )
)

(define (FIsNInvalid n)
    (set-empty? (hash-ref mpSumSetIj n (set)))
)

(let go ([lN lNRemain] [iN 0])
    (cond
        ([empty? lN] #f)
        (else
            (begin
                (define n (first lN))
                (define lNNext (rest lN))
                (cond
                    ([FIsNInvalid n] n)
                    (else
                        (begin
                            (UpdateMp iN n)
                            (go lNNext (modulo (add1 iN) cnWin))
                        )
                    )
                ) 
            )
        )
    )
)