#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)
(require racket/match)
(require racket/string)
(require yaml)

(define (MpUpdate mpNIJ n i)
    (define ij (hash-ref mpNIJ n (cons -1 -1)))
    (hash-set mpNIJ n (cons (add1 i) (car ij)))
)

(define (Run1 cRun lN)
    (define mpNIJSeed
        (for/fold
            ([mpNIJ (hash)] #:result mpNIJ)
            ([(n i) (in-indexed (in-list lN))])
            (MpUpdate mpNIJ n i)
        ))

    (for/fold
        ([n (last lN)] [mpNIJ mpNIJSeed] #:result n)
        ([iRun (in-range (length lN) cRun)])
        (define ij (hash-ref mpNIJ n (cons -1 -1)))
        (define nNew (if (negative? (cdr ij)) 0 (- (car ij) (cdr ij))))
        (values nNew (MpUpdate mpNIJ nNew iRun))
    )    
)

(define (Solve1 strFile)
    (define pathData
        (build-path
            g_pathCode
            strFile))

    (define yFile (file->yaml pathData))
    (define cRun (first yFile))
    (define lStrSeed (rest yFile))
    (define llNSeed (map (lambda (str) (map string->number (string-split str ","))) lStrSeed))

    (printf "cRun: ~v~n" cRun)
    (for ([lN (in-list llNSeed)] [str (in-list lStrSeed)])
        (printf "seed: ~v end: ~v~n" str (Run1 cRun lN))
    )
)

(define (Solve2 strFile)
    (define pathData
        (build-path
            g_pathCode
            strFile))

    (define lStrFile (file->lines pathData))
    (define lN (map string->number lStrFile))
    lN
)

(Solve1 "15.yaml")
;(Solve1 "15.txt")
;(Solve2 "15-.txt")
;(Solve2 "15.txt")

