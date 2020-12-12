#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

; (require racket/runtime-path)
; (define-runtime-path pathData "1.txt")
(define g_pathData
    (build-path
        g_pathCode
        "1.txt"))

(define g_lStr (file->lines g_pathData))
(define g_lN (sort (map string->number g_lStr) <))
(define g_nTarget 2020)

(define (match2 i_lN i_nTarget)
    (let check ([lN i_lN])
      (define nFirst (first lN))
      (define nLast (last lN))
      (define nSum (+ nFirst nLast))
      (cond
        [(<= (length lN) 1) '()]
        [(< nSum i_nTarget) (check (drop lN 1))]
        [(> nSum i_nTarget) (check (drop-right lN 1))]
        [else (list nFirst nLast)])))

; (define p2 (match2 g_lN g_nTarget))
; (displayln p2)
; (displayln (apply * p2))

(define (remove-nth lst i)
  (let aux ([lst lst] [i i] [acc '()])
    (cond [(null? lst) (reverse acc)]               ;; what if (< (length lst) i) 
          [(<= i 0) (append-reverse acc (cdr lst))] ;; what if (< i 0)
          [else (aux (cdr lst) (sub1 i) (cons (car lst) acc))])))

(define (match3 i_lN i_nTarget)
    (let check ([lNPre '()] [n (first i_lN)] [lNPost (rest i_lN)])
      (define lNLess (append lNPre lNPost))
      (define nTargetLess (- i_nTarget n))
      (define lNMatch2 (match2 lNLess nTargetLess))
      (cond
        [(< (length lNPost) 1) '()]
        [(= (length lNMatch2) 2) (append lNMatch2 (list n))]
        [else (check (append lNPre (list n)) (first lNPost) (rest lNPost))])))

(define p3 (match3 g_lN g_nTarget))
(displayln p3)
(displayln (apply * p3))

