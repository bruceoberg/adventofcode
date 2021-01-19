#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)
(require racket/match)

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

    lInst
)

(Solve1 "14-.txt")
(Solve1 "14.txt")
