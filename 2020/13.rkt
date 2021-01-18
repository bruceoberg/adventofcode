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
        "13.txt"))

(define g_lStrFile (file->lines g_pathData))

(define g_tMe (string->number (first g_lStrFile)))
(define g_lStrId (string-split (second g_lStrFile) ","))
(define g_lIdRaw (map string->number g_lStrId))

; part one

; (define g_lId (filter number? g_lIdRaw))

; ;g_tMe
; ;g_lId

; (define (IdntFromId id)
;     (define tDiv (+ g_tMe (sub1 id)))
;     (define rid (quotient tDiv id))
;     (cons (* rid id) id)
; )

; (define g_lIdnt (map IdntFromId g_lId))
; (define g_idntTake (argmin car g_lIdnt))
; (define g_dt (- (car g_idntTake) g_tMe))
; (define g_id (cdr g_idntTake))

; g_tMe
; g_idntTake
; (* g_dt g_id)

; part two

(define g_lIddtRaw (for/list ([id (in-list g_lIdRaw)] [dt (in-naturals)]) (cons id dt)))

(define g_lIddtAll (filter (lambda (iddt) (number? (car iddt))) g_lIddtRaw))
(define g_lIddt (sort g_lIddtAll #:key car >))

(define (iddt-legit? t iddt) (equal? (remainder t (car iddt)) (cdr iddt)))
;(define (legit? t) (apply and (map (lambda (iddt) (iddt-legit? t iddt)) g_lIddt)))

g_lIddt
; (define g_lPddt
;     (for/fold
;         ([lPddt '()] [p 1] #:result (reverse lPddt))
;         ([iddt (in-list g_lIddt)])
            
;         (define id (car iddt))
;         (define dT (cdr iddt))
;         (define pNext (* id p))
;         (define dTChk (remainder (- dT) id))
;         (values (cons (list pNext id dTChk) lPddt) pNext)
;     )
; )

(for/fold
    ([t 0] [dT 1] #:result t)
    ([iddt (in-list g_lIddt)])
    (define id (car iddt))
    (define dTChk (modulo (- (cdr iddt)) id))
    (let go ([tI t])
        (if (= (remainder tI id) dTChk)
            (values tI (* dT id))
            (go (+ tI dT))
        )
    )
)