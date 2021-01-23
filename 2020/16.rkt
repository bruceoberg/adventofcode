#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)
(require racket/match)
(require racket/string)
(require racket/class)
(require yaml)

(define mm%
  (class* object% (printable<%>)
    (super-new)
    (field (mpLSetR (make-hash)))
    (field (mpRSetL (make-hash)))

    (define/public (add l r)
        (define setR (hash-ref! mpLSetR l (mutable-set)))
        (set-add! setR r)
        (define setL (hash-ref! mpRSetL r (mutable-set)))
        (set-add! setL l)
    )

    (define/public (l-ref l) (hash-ref mpLSetR l (set)))
    (define/public (r-ref r) (hash-ref mpRSetL r (set)))
  
    ;; implement printable interface
    (define/public (custom-print port quoting-depth)
      (print (~a "l->r:" mpLSetR " r->l:" mpRSetL "\n") port))
    (define/public (custom-write port)
      (write (~a "l->r:" mpLSetR " r->l:" mpRSetL "\n") port))
    (define/public (custom-display port)
      (display (~a "l->r:" mpLSetR " r->l:" mpRSetL "\n") port))
  )
)

(define (y->mm y)
    (define mm (new mm%))

    (for ([(strField strRange) (in-hash y)])
        (for ([strClause (string-split strRange " or ")])
            (match strClause
                ([regexp #px"^([0-9]+)-([0-9]+)$" (list _ strFirst strLast)]
                    (for ([n (in-range (string->number strFirst) (add1 (string->number strLast)))])
                        (send mm add strField n)
                    )
                )
                (_ (display "BAD ") (displayln strClause))
            )
        )
    )

    mm
)

(define (Solve1 strFile)
    (define pathData
        (build-path
            g_pathCode
            strFile))

    (define yFile (file->yaml pathData))

    (define tickMe (hash-ref yFile "your ticket"))
    (define lTickThem (hash-ref yFile "nearby tickets"))
    
    (hash-remove! yFile "your ticket")
    (hash-remove! yFile "nearby tickets")

    (define mm (y->mm yFile))

    (define (n-bad? n) (set-empty? (send mm r-ref n)))
    
    (for/fold
        ([sum 0] #:result sum)
        ([tick (in-list lTickThem)])
        (define lNBad (filter n-bad? tick))
        ;(println lNBad)
        (+ sum (apply + lNBad))
    )
)

(define (Solve2 strFile)
    (define pathData
        (build-path
            g_pathCode
            strFile))

    (define yFile (file->yaml pathData))

    (define tickMe (hash-ref yFile "your ticket"))
    (define lTickThem (hash-ref yFile "nearby tickets"))
    
    (hash-remove! yFile "your ticket")
    (hash-remove! yFile "nearby tickets")

    (define mm (y->mm yFile))

    (define (n-bad? n) (set-empty? (send mm r-ref n)))
    (define (tick-ok? tick) (empty? (filter n-bad? tick)))

    (define lTickOk (filter tick-ok? lTickThem))

;    (println (send mm r-ref 9))

    (define mpINSetField
        (for/fold
            ([mpINSetField (make-hash)] #:result mpINSetField)
            ([tick (in-list lTickOk)])
            (for ([iN (in-naturals)] [n (in-list tick)])
;                (println (~a iN " " n))
                (define setFieldN (send mm r-ref n))
;               (println setFieldN)
                (define setFieldIN (hash-ref! mpINSetField iN (set-copy setFieldN)))
;                (println setFieldIN)
                (set-intersect! setFieldIN setFieldN)
;                (println setFieldIN)
            )
            mpINSetField
        )
    )

    (define mpINField
        (let go ([lINS (hash->list mpINSetField)] [mpINField (make-hash)] [setFieldDone (mutable-set)])
            (cond
                ([empty? lINS] mpINField)
                (else
                    (begin
                        (define (one-field? ins) (= 1 (set-count (cdr ins))))
                        (define-values (lINSOne lINSMore) (partition one-field? lINS))
                        (for ([ins lINSOne])
                            (define iN (car ins))
                            (define strField (set-first (cdr ins)))
                            (hash-set! mpINField iN strField)
                            (set-add! setFieldDone strField)
                        )
                        (for ([ins lINSMore]) (set-subtract! (cdr ins) setFieldDone))
                        (go lINSMore mpINField setFieldDone)
                    )
                )
            )
        )
    )

    (define (is-dep? ins) (string-prefix? (cdr ins) "departure"))
    (define lINDep (map car (filter is-dep? (hash->list mpINField))))
    (define vNMe (list->vector tickMe))
    (define lNDep (map (lambda (iN) (vector-ref vNMe iN)) lINDep))
    lNDep
    (apply * lNDep)
    ;(for ([(iN setField) (in-hash mpINSetField)]) (println (~a "iN:" iN " s:" (set-count setField))))
    ;mpINSetField
)

;(Solve1 "16-.yaml")
;(Solve1 "16.yaml")
;(Solve2 "16-.yaml")
;(Solve2 "16--.yaml")
(Solve2 "16.yaml")

