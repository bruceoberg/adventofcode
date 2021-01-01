#lang typed/racket



;(define g_pathCode : String (this-expression-source-directory))

;(require srfi/1) ; for drop-right
;(require srfi/13) ; for string-take etc
;(require racket/trace)

; day 8

(define g_pathData : Path
    (build-path
        (find-system-path 'orig-dir)
        "2020"
        "8.txt"))

(define lStrFile : (Listof String) (file->lines g_pathData #:mode 'text))

(: string->integer (-> String Integer))
(define (string->integer str)
  (define n (string->number str))
  (if (exact-integer? n) n (raise 'not-an-integer 0))
)

(struct SInst (
    [strOp : String]
    [nArg : Integer])
    #:transparent
)

(: InstFromStr (-> String SInst))
(define (InstFromStr str)
    (define lStr : (Listof String) (string-split str " "))
    (SInst (first lStr) (string->integer (second lStr)))
)

(define aInst : (Vectorof SInst) (list->vector (map InstFromStr lStrFile)))

(let go ([iInst : Integer 0] [nAcc : Integer 0] [setIInstSeen : (Setof Integer) (set)])
    (cond
        ([set-member? setIInstSeen iInst] nAcc)
        (else
            (begin
                (define setInstSeenNext (set-add setIInstSeen iInst))
                (define inst : SInst (vector-ref aInst iInst))
                (displayln (list iInst nAcc inst))
                (define strOp (SInst-strOp inst))
                (define nArg (SInst-nArg inst))
                (cond
                    ([equal? strOp  "nop"] (go (add1 iInst) nAcc setInstSeenNext))
                    ([equal? strOp "acc"] (go (add1 iInst) (+ nAcc nArg) setInstSeenNext))
                    ([equal? strOp "jmp"] (go (+ iInst nArg) nAcc setInstSeenNext))
                    (else (raise 'unknown-opcode #f))
                )
            )
        )
    )
)