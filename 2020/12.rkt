#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)
(require racket/match)

; (require racket/runtime-path)
; (define-runtime-path pathData "1.txt")
(define g_pathData
    (build-path
        g_pathCode
        "12-.txt"))

(define g_lStrFile (file->lines g_pathData))

(struct SInst (chOp nArg) #:transparent)

(define (InstFromStr str)
    (define pat #px"^([NSEWLRF]{1})(.*)$")
    (define lStr (regexp-match pat str))
    (SInst (second lStr) (string->number (third lStr)))
)

(define g_lInst (map InstFromStr g_lStrFile))

(let ([pos 0+0i] [deg 0])
    (for ([inst g_lInst])
        (printf "pos: ~v deg: ~v inst: ~v~n" pos deg inst)
        (match (SInst-chOp inst)
            ("N" (set! pos (+ pos (* (SInst-nArg inst) +0+1i))))
            ("S" (set! pos (+ pos (* (SInst-nArg inst) +0-1i))))
            ("E" (set! pos (+ pos (* (SInst-nArg inst) +1+0i))))
            ("W" (set! pos (+ pos (* (SInst-nArg inst) -1+0i))))
            ("L" (set! deg (+ deg (SInst-nArg inst))))
            ("R" (set! deg (- deg (SInst-nArg inst))))
            ("F" (set! pos (+ pos (make-polar (SInst-nArg inst) (degrees->radians deg)))))
        )
        (set! pos (make-rectangular (round (real-part pos)) (round (imag-part pos))))
    )
    (define man (+ (abs (real-part pos)) (abs (imag-part pos))))
    (printf "pos: ~v deg: ~v man: ~v~n" pos deg man)
)
