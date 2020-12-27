#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right
(require srfi/13) ; for string-take etc

(require racket/trace)

; day 6

(define g_pathData
    (build-path
        g_pathCode
        "6.txt"))

(define strFile (file->string g_pathData #:mode 'text))
(define lStrFile (string-split strFile "\n\n"))
(define llStr (map (lambda (str) (string-split str "\n")) lStrFile))

(define (SetChFromStr str) (list->set (string->list str)))
(define (LSetChFromLStr lStr) (map SetChFromStr lStr))

(define llSetCh (map LSetChFromLStr llStr))

; (define (SetChUnionLSetCh lSetCh) (apply set-union lSetCh))
; (define lSetChUnion (map SetChUnionLSetCh llSetCh))
; (define lcChUnion (map set-count lSetChUnion))
; (apply + lcChUnion)

(define (SetChIntersectLSetCh lSetCh) (apply set-intersect lSetCh))
(define lSetChIntersect (map SetChIntersectLSetCh llSetCh))
(define lcChIntersect (map set-count lSetChIntersect))
(apply + lcChIntersect)
