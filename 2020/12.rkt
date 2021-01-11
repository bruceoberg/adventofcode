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
        "12.txt"))

(define g_lStrFile (file->lines g_pathData))

(struct SInst (chOp nArg) #:transparent)

(define (InstFromStr str)
    (define pat #px"^([NSEWLRF]{1})(.*)$")
    (define lStr (regexp-match pat str))
    (SInst (second lStr) (string->number (third lStr)))
)

(define g_lInst (map InstFromStr g_lStrFile))

(let ([pos 0+0i] [dPos 10+1i])
    (for ([inst g_lInst])
        (printf "pos: ~v dPos: ~v inst: ~v~n" pos dPos inst)
        (match (SInst-chOp inst)
            ("N" (set! dPos (+ dPos (* (SInst-nArg inst) +0+1i))))
            ("S" (set! dPos (+ dPos (* (SInst-nArg inst) +0-1i))))
            ("E" (set! dPos (+ dPos (* (SInst-nArg inst) +1+0i))))
            ("W" (set! dPos (+ dPos (* (SInst-nArg inst) -1+0i))))
            ("L" (set! dPos (make-polar
                                (magnitude dPos)
                                (+
                                    (angle dPos)
                                    (degrees->radians
                                        (SInst-nArg inst))))))
            ("R" (set! dPos (make-polar
                                (magnitude dPos)
                                (-
                                    (angle dPos)
                                    (degrees->radians
                                        (SInst-nArg inst))))))
            ("F" (set! pos (+ pos (* (SInst-nArg inst) dPos))))
        )
        (set! pos (make-rectangular (round (real-part pos)) (round (imag-part pos))))
        (set! dPos (make-rectangular (round (real-part dPos)) (round (imag-part dPos))))
    )
    (define man (+ (abs (real-part pos)) (abs (imag-part pos))))
    (printf "pos: ~v dPos: ~v man: ~v~n" pos dPos man)
)
