#lang racket

(require mzlib/etc)
(define pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)

; (require racket/runtime-path)
; (define-runtime-path pathData "1.txt")
(define pathData
    (build-path
        pathCode
        "5.txt"))

(define lStrFile (file->lines pathData))

(define (ChBinFromChZone ch)
    (cond
        [(equal? ch #\F) #\0]
        [(equal? ch #\B) #\1]
        [(equal? ch #\L) #\0]
        [(equal? ch #\R) #\1])
)

(define (StrBinFromStrZone strZone)
    (list->string (map ChBinFromChZone (string->list strZone)))
)

(define (NFromStrBin strBin)
    (string->number strBin 2)
)

(define (NFromStrZone strZone)
    (NFromStrBin (StrBinFromStrZone strZone))
)

(define lN (map NFromStrZone lStrFile))
;(apply max lN)
;(map NFromStrZone (list "FBFBBFFRLR" "BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"))

(define (RcFromN n)
    (define-values (r c) (quotient/remainder n 8))
    (list r c)
)

(define g_lRc (map RcFromN (sort lN <)))
;(take lRc 5)

(define (MpRSetC in_lRc)
    (let go ([mpRSetC (make-immutable-hash)] [lRc in_lRc])
        (cond
            ([empty? lRc] mpRSetC)
            (else
                (begin
                    (define rc (first lRc))
                    (define lRcNext (rest lRc))
                    (match-define (list r c) rc)
                    (define setC (hash-ref mpRSetC r (set)))
                    (define setCNext (set-add setC c))
                    (define mpRSetCNext (hash-set mpRSetC r setCNext))
                    (go mpRSetCNext lRcNext)
                )))))

(define g_mpRSetC (MpRSetC g_lRc))
(define g_lR (sort (hash-keys g_mpRSetC) <))
(define setCAll (list->set (range 8)))

(let go ([lR g_lR] [fHaveFull #f])
    (define r (first lR))
    (define lRNext (rest lR))
    (define setC (hash-ref g_mpRSetC r (set)))
    (define setCEmpty (set-subtract setCAll setC))
    (define fIsFull (set-empty? setCEmpty))
    (define fHaveFullNext (or fHaveFull fIsFull))
    (cond
        [(and fHaveFull (not fIsFull)) (+ (set-first setCEmpty) (* 8 r))]
        [else (go lRNext fHaveFullNext)]
    )
)

