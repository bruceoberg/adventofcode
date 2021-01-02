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

(struct SRes (
    [nAcc : Integer]
    [setIInstSeen : (Setof Integer)])
    #:transparent
)

(: ResRunAInst (-> (Vectorof SInst) SRes))
(define (ResRunAInst aInst)
    (let go ([iInst : Integer 0] [nAcc : Integer 0] [setIInstSeen : (Setof Integer) (set)])
        (cond
            ([equal? iInst (vector-length aInst)] (SRes nAcc (set)))
            ([set-member? setIInstSeen iInst] (SRes nAcc setIInstSeen))
            (else
                (begin
                    (define setInstSeenNext (set-add setIInstSeen iInst))
                    (define inst : SInst (vector-ref aInst iInst))
                    ;(displayln (list iInst nAcc inst))
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
)

(define res (ResRunAInst aInst))
(SRes-setIInstSeen res)

(define mpOpOpTweak : (HashTable String String) (hash "jmp" "nop" "nop" "jmp"))
(define aInstTweak (vector-copy aInst))

(let go ([setIInstTweak (SRes-setIInstSeen res)])
    (cond
        ([set-empty? setIInstTweak] #f)
        (else
            (begin
                (define iInstTweak (set-first setIInstTweak))
                (define setIInstTweakNext (set-rest setIInstTweak))
                ;(displayln "")
                ;(displayln iInstTweak)
    
                (define instOrig (vector-ref aInstTweak iInstTweak))

                (define strOpOrig (SInst-strOp instOrig))
                (define strOpTweak (hash-ref mpOpOpTweak strOpOrig (λ () strOpOrig)))
                (define nArgOrig (SInst-nArg instOrig))
                (vector-set! aInstTweak iInstTweak (SInst strOpTweak nArgOrig))
    
                (define resTweak (ResRunAInst aInstTweak))
                (define fTweakLooped (not (set-empty? (SRes-setIInstSeen resTweak))))

                (vector-set! aInstTweak iInstTweak instOrig)

                (if fTweakLooped
                    (go setIInstTweakNext)
                ; else
                    (SRes-nAcc resTweak)
                )
            )
        )
    )
)
