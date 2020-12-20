#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)

; day 2

; (define g_pathData
;     (build-path
;         g_pathCode
;         "2.txt"))

; (define g_lStr (file->lines g_pathData))

; (define (RawFromStr str)
;     (rest (regexp-match #rx"^([0-9]+)-([0-9]+) (.): (.*)$" str))
; )

; (define (RecFromRaw strMin strMax strCh strPassword)
;     (list
;         (string->number strMin)
;         (string->number strMax)
;         (string-ref strCh 0)
;         strPassword
;     )
; )

; (define (RecFromStr str)
;     (apply RecFromRaw (RawFromStr str))
; )

; (define (CChMatchStr ch str)
;     (length (filter (lambda (chStr) (equal? ch chStr)) (string->list str)))
; )

; (define (FInRange c cFirst cLast)
;     (and
;         (>= c cFirst)
;         (<= c cLast)
;     )
; )

; (define (FIsChAtPos str ch nCh) ; nCh is 1 based
;     (equal? ch (string-ref str (- nCh 1)))
; )

; (define (ResFromRec rec)
;     (apply
;         (lambda (cChFirst cChLast ch str)
;             (define cCh (CChMatchStr ch str))
;             (define fInRange (FInRange cCh cChFirst cChLast))
;             (define fIsChFirst (FIsChAtPos str ch cChFirst))
;             (define fIsChLast (FIsChAtPos str ch cChLast))
;             (define fInOneMatched (not (boolean=? fIsChFirst fIsChLast))) ; aka xor
;             (list fInOneMatched fIsChFirst fIsChLast fInRange cCh cChFirst cChLast ch str)
;         )
;         rec
;     )
; )

; (define (ResFromStr str)
;     (ResFromRec (RecFromStr str))
; )

; ;(ResFromRec (RecFromStr (first g_lStr)))

; (filter
;     (lambda (rec) (first rec))
;     (map ResFromStr (take g_lStr 10))
; )

; (length
;     (filter
;         (lambda (rec) (first rec))
;         (map ResFromStr g_lStr)
;     )
; )
