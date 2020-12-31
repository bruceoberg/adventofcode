#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

;(require srfi/1) ; for drop-right
;(require srfi/13) ; for string-take etc

(require racket/trace)

; day 7

(define g_pathData
    (build-path
        g_pathCode
        "7.txt"))

(define lStrFile (file->lines g_pathData #:mode 'text))
(define lStrClean1 (map (lambda (str) (string-replace str " bags" "")) lStrFile))
(define lStrClean2 (map (lambda (str) (string-replace str " bag" "")) lStrClean1))
(define lStrClean3 (map (lambda (str) (string-replace str "." "")) lStrClean2))

(define lSS (map (lambda (str) (string-split str " contain ")) lStrClean3))
(define lSL (map (lambda (ss) (cons (first ss) (string-split (last ss) ", "))) lSS))

(define (BicFromStr str)
    (define lStr (string-split str " "))
    (define c (string->number (first lStr)))
    (define strBi (string-join (rest lStr) " "))
    (list strBi c)
)

(define (valid-bic? bic) (second bic))

(define (LBobicFromSl sl)
    (define strBo (car sl))
    (define lStrBic (cdr sl))
    (define lBicRaw (map BicFromStr lStrBic))
    (define lBic (filter valid-bic? lBicRaw))
    (map (lambda (bic) (list strBo (first bic) (second bic))) lBic)
)

(define llBobic (map LBobicFromSl lSL))
(define lBobic (apply append llBobic))

(define (BibocFromBobic bobic) (list (second bobic) (first bobic) (third bobic)))

(define lBiboc (map BibocFromBobic lBobic))

(define (MpBbc in_lBbc)
    (let go ([mpBbc (make-hash)] [lBbc in_lBbc])
        (cond
            ([empty? lBbc] mpBbc)
            (else
                (begin
                    (define bbc (first lBbc))
                    (define lBbcNext (rest lBbc))

                    (match-define (list strB1 strB2 c) bbc)

                    (define mpBc1 (hash-ref! mpBbc strB1 (make-hash)))
                    (define mpBc2 (hash-ref! mpBbc strB2 (make-hash)))
                    (hash-set! mpBc1 strB2 c)

                    (go mpBbc lBbcNext))))))



(define mpBobic (MpBbc lBobic))

; part 1

; (define (SetBFromBBbc strB mpBbc)
;     (define setBReturn (mutable-set))
;     (let go ([setBTodo (set strB)] [setBDone (set)])
;         ; (displayln "setBChk")
;         ; (displayln setBChk)
;         ; (displayln "setB")
;         ; (displayln setB)
;         (cond
;             ; ([>= (set-count setB) 2] (exit 1))
;             ([set-empty? setBTodo] setBReturn)
;             (else
;                 (begin
;                     (define strBCheck (set-first setBTodo))
;                     (define setBRest (set-rest setBTodo))
;                     (define setBDoneNext (set-add setBDone strBCheck))

;                     (define mpBc (hash-ref mpBbc strBCheck (make-hash)))
;                     (define lBRef (hash-keys mpBc))
;                     (define setBRef (list->set lBRef))
;                     (set-union! setBReturn setBRef)
;                     ; (displayln "setBRef")
;                     ; (displayln setBRef)
;                     (define setBRestPlusRef (set-union setBRest setBRef))
;                     (define setBTodoNext (set-subtract setBRestPlusRef setBDoneNext))
;                     (go setBTodoNext setBDoneNext)
;                 )
;             )
;         )
;     )
; )

;(hash-count (hash-ref mpBobic "muted tomato"))
;(define setBo (SetBFromBBbc "shiny gold" mpBiboc))
;(displayln (set-count setBo))

; part 2

(define (LBBw lBoMpBic)
    (define (FIsMpEmpty boMpBic) (hash-empty? (cdr boMpBic)))
    (define-values (lBoMpBicTail lBoMpBicHead) (partition FIsMpEmpty lBoMpBic))
    (define lBTail (map car lBoMpBicTail))
    
    (let go ([setBTail (list->set lBTail)] [lBoMpBic lBoMpBicHead] [lBbwReverse '()])
        (cond
;            ([>= (length lBbwReverse) 3] (exit 1))
            ([set-empty? setBTail] (reverse lBbwReverse))
            (else
                (begin
                    (define strB (set-first setBTail))
                    
                    (define setBTailRest (set-rest setBTail))
                    (define lBbwReverseNext (cons strB lBbwReverse))

                    (define (FIsMpEmptyAfterRemove boMpBic)
                        (define mpBic (cdr boMpBic))
                        (hash-remove! mpBic strB)
                        (hash-empty? mpBic)
                    )

                    (define-values (lBoMpBicTail lBoMpBicHead) (partition FIsMpEmptyAfterRemove lBoMpBic))
                    (define setBTailNext (set-union setBTailRest (list->set (map car lBoMpBicTail))))

                    (go setBTailNext lBoMpBicHead lBbwReverseNext))))))

(define (MpBC lBobic)
    (define mpBobic (MpBbc lBobic))
    (define lBoMpBic (hash->list mpBobic))
    (define lBBw (LBBw lBoMpBic))
    (define mpBC (make-hash))
    (define mpBobic2 (MpBbc lBobic))
    (let go ([lB lBBw])
        (cond
;            ([>= (hash-count mpBC) 15] (exit 1))
            ([empty? lB] mpBC)
            (else
                (begin
                    (define strB (first lB))
                    (define lBNext (rest lB))

                    (define mpBic (hash-ref mpBobic2 strB))
                    ; (displayln strB)
                    ; (displayln mpBic)
                    (define (CFromBic strBi c) (* c (hash-ref mpBC strBi)))
                    (define c (if (hash-empty? mpBic) 1 (apply + (cons 1 (hash-map mpBic CFromBic)))))
                    (hash-set! mpBC strB c)
                    ;(displayln mpBC)
                    (go lBNext)
                )))))

(define mpBC (MpBC lBobic))

(sub1 (hash-ref mpBC "shiny gold"))

