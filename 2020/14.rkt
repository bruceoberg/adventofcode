#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)
(require racket/match)
(require racket/string)

(struct SInst (strOp mem strArg) #:transparent)

(define (InstFromStr str)
    (match str
        ([regexp #px"^mask = ([01X]{36})$" (list _ strMask)]
            (SInst "mask" 0 strMask))
        ([regexp #px"^mem\\[([0-9]+)\\] = ([0-9]+)$" (list _ strMem strVal)]
            (SInst "mem" (string->number strMem) (~r (string->number strVal) #:base 2 #:min-width 36 #:pad-string "0")))
        (_ (display "BAD ") (displayln str))
    )
)

(define (StrCombine strMem strMask strVal)
    (list->string
        (for/list
            (
                [chMem (in-string strMem)]
                [chMask (in-string strMask)]
                [chVal (in-string strVal)]
            )
            (if (equal? chMask #\X) chVal chMask)
        )
    )
)

(define (MpMemStr lInst)
    (for/fold
        (
            [mpMemStr (make-immutable-hash)]
            [strMask (make-string 36 #\X)]
            #:result mpMemStr
        )
        
        ([inst lInst])
        
        (match-define (SInst strOp mem strArg) inst)
        (match strOp
            ["mask" (values mpMemStr strArg)]
            ["mem"
                (define strMem (hash-ref mpMemStr mem (make-string 36 #\0)))
                (define strMemNew (StrCombine strMem strMask strArg))
                (values (hash-set mpMemStr mem strMemNew) strMask)
            ]
            [_ (values mpMemStr strMask)]
        )
    )
)

(define (weave lX lY)
  (match (list lX lY)
    [(list (cons x lX) (cons y lY)) (cons x (cons y (weave lX lY)))]
    [(list '() lY)                  lY]
    [(list lX '())                  lX]))

(define (LStrExpandFloat strMask)
    (define lStr (string-split strMask "X" #:trim? #f))
    (define cGap (sub1 (length lStr)))
    (match cGap
        [0 lStr]
        [_
            (for/list ([i (in-range (expt 2 cGap))])
                (define strI (~r i #:base 2 #:min-width cGap #:pad-string "0"))
                (define lStrGap (map string (string->list strI)))
                (string-join (weave lStr lStrGap) "")
            )
        ]
    )
)

(define (StrFloat strAddr strMask)
    (list->string
        (for/list
            (
                [chAddr (in-string strAddr)]
                [chMask (in-string strMask)]
            )
            (match chMask
                [#\0 chAddr]
                [#\1 #\1]
                [#\X #\X]
            )
        )
    )
)

(define (MpMemStr2 lInst)
    (for/fold
        (
            [mpMemStr (make-immutable-hash)]
            [strMask (make-string 36 #\X)]
            #:result mpMemStr
        )
        
        ([inst lInst])
        
        (match-define (SInst strOp mem strArg) inst)
        (match strOp
            ["mask" (values mpMemStr strArg)]
            ["mem"
                (define strAddr (~r mem #:base 2 #:min-width 36 #:pad-string "0"))
                (define strFloat (StrFloat strAddr strMask))
                (define lStrAddr (LStrExpandFloat strFloat))

                (define mpMemStrNew
                    (for/fold ([mpMemStrI mpMemStr] #:result mpMemStrI) ([strAddr lStrAddr])
                        (define mem (string->number (string-append "#b" strAddr)))
                        (define strMem (hash-ref mpMemStrI mem (make-string 36 #\0)))
                        (hash-set mpMemStrI mem strArg)
                    ))
                (values mpMemStrNew strMask)
            ]
            [_ (values mpMemStr strMask)]
        )
    )
)

(define (Solve1 strFile)
    (define pathData
        (build-path
            g_pathCode
            strFile))

    (define lStrFile (file->lines pathData))
    (define lInst (map InstFromStr lStrFile))
    (define mpMemStr (MpMemStr lInst))
    (define lStrVal (hash-values mpMemStr))
    (define lN (map (lambda (str) (string->number (string-append "#b" str))) lStrVal))
    (apply + lN)
)

(define (Solve2 strFile)
    (define pathData
        (build-path
            g_pathCode
            strFile))

    (define lStrFile (file->lines pathData))
    (define lInst (map InstFromStr lStrFile))

    (define mpMemStr (MpMemStr2 lInst))
    (define lStrVal (hash-values mpMemStr))
    (define lN (map (lambda (str) (string->number (string-append "#b" str))) lStrVal))
    (apply + lN)
)

;(Solve1 "14-.txt")
;(Solve1 "14.txt")
(Solve2 "14--.txt")
(Solve2 "14.txt")

