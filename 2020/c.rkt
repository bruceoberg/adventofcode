#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

; ; (require racket/runtime-path)
; ; (define-runtime-path pathData "1.txt")
; (define g_pathData
;     (build-path
;         g_pathCode
;         "1.txt"))

; (define g_lStr (file->lines g_pathData))
; (define g_lN (sort (map string->number g_lStr) <))
; (define g_nTarget 2020)

; (define (match2 i_lN i_nTarget)
;     (let check ([lN i_lN])
;       (define nFirst (first lN))
;       (define nLast (last lN))
;       (define nSum (+ nFirst nLast))
;       (cond
;         [(<= (length lN) 1) '()]
;         [(< nSum i_nTarget) (check (drop lN 1))]
;         [(> nSum i_nTarget) (check (drop-right lN 1))]
;         [else (list nFirst nLast)])))

; ; (define p2 (match2 g_lN g_nTarget))
; ; (displayln p2)
; ; (displayln (apply * p2))

; (define (remove-nth lst i)
;   (let aux ([lst lst] [i i] [acc '()])
;     (cond [(null? lst) (reverse acc)]               ;; what if (< (length lst) i) 
;           [(<= i 0) (append-reverse acc (cdr lst))] ;; what if (< i 0)
;           [else (aux (cdr lst) (sub1 i) (cons (car lst) acc))])))

; (define (match3 i_lN i_nTarget)
;     (let check ([lNPre '()] [n (first i_lN)] [lNPost (rest i_lN)])
;       (define lNLess (append lNPre lNPost))
;       (define nTargetLess (- i_nTarget n))
;       (define lNMatch2 (match2 lNLess nTargetLess))
;       (cond
;         [(< (length lNPost) 1) '()]
;         [(= (length lNMatch2) 2) (append lNMatch2 (list n))]
;         [else (check (append lNPre (list n)) (first lNPost) (rest lNPost))])))

; (define p3 (match3 g_lN g_nTarget))
; (displayln p3)
; (displayln (apply * p3))

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

; day 3

(define g_pathData
    (build-path
        g_pathCode
        "3.txt"))

(define g_lStr (file->lines g_pathData))

(define g_laCh (map (lambda (str) (list->vector (string->list str)) g_lStr)
(define g_aaCh (list->vector g_laCh)) 
(define g_xMax (length (first g_lStr)))
(define g_yMax (length g_lStr))
(define g_dx 3)
(define g_dy 1)

(let go  ([x g_dx] [y g_dy] [cT 0])
    (define xW (mod x g_xMax))
    (cond
        [(< y g_yMax) ]
        [else cT])))
