#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)

; (require racket/runtime-path)
; (define-runtime-path pathData "1.txt")
(define g_pathData
    (build-path
        g_pathCode
        "17.txt"))

(define g_lStr (file->lines g_pathData))

; from https://github.com/jeapostrophe/exp/blob/master/life.rkt

(require racket/match
         racket/unsafe/ops
         racket/performance-hint
         racket/vector)

(define dSEdge 10)

(struct SDish (dS aBCur aBNxt mpIBLIBAdj) #:mutable)

(define (make-aB dS) (make-bytes (expt dS 4)))
(define (make-iB dS x y z w)
    (unsafe-fx+
        x
        (unsafe-fx*
            dS
            (unsafe-fx+
                y
                (unsafe-fx*
                    dS
                    (unsafe-fx+
                        z
                        (unsafe-fx*
                            dS
                            w)))))))
(define-inline (aB-set! aB dS x y z w b)
  (unsafe-bytes-set! aB (make-iB dS x y z w) b))
(define-inline (aB-ref aB dS x y z w)
  (unsafe-fx= 1 (unsafe-bytes-ref aB (make-iB dS x y z w))))
(define-inline (aB-raw-ref aB iB)
  (unsafe-bytes-ref aB iB))

(define (MpIBLIBAdj dS)
    (define lXyzwAll (for*/list ([x (in-range -1 2)] [y (in-range -1 2)] [z (in-range -1 2)] [w (in-range -1 2)]) (list x y z w)))
    (define lXyzwAdj (filter (lambda (xyzw) (or (not (zero? (first xyzw))) (not (zero? (second xyzw))) (not (zero? (third xyzw))) (not (zero? (fourth xyzw))))) lXyzwAll))
    (define (FXyzwOk xyzw)
      (define x (first xyzw))
      (define y (second xyzw))
      (define z (third xyzw))
      (define w (fourth xyzw))
      (and (>= x 0) (< x dS) (>= y 0) (< y dS) (>= z 0) (< z dS) (>= w 0) (< w dS)))
    (define (IbFromXyzw xyzw) (make-iB dS (first xyzw) (second xyzw) (third xyzw) (fourth xyzw)))
    (define (XyzwAdd xyzw1 xyzw2) (list (+ (first xyzw1) (first xyzw2)) (+ (second xyzw1) (second xyzw2)) (+ (third xyzw1) (third xyzw2)) (+ (fourth xyzw1) (fourth xyzw2))))
    (for*/vector
      (
          [w (in-range dS)]
          [z (in-range dS)]
          [y (in-range dS)]
          [x (in-range dS)])
      (define xyzwCur (list x y z w))
      (define lXyzwNear (map (lambda (xyzwAdj) (XyzwAdd xyzwAdj xyzwCur)) lXyzwAdj))
      (define lXyzwOk (filter FXyzwOk lXyzwNear))
      (define (IbFromXyzw xyzw) (make-iB dS (first xyzw) (second xyzw) (third xyzw) (fourth xyzw)))
      (sort (map IbFromXyzw lXyzwOk) <)))

(define (lstring->dish lStr)
  (define dX (length lStr))
  (define dY (apply max (map string-length lStr)))
  (define dSPad (* 2 dSEdge))
  (define dS (+ dSPad (max dX dY)))
  (define sCenter (quotient dS 2))
  (define aBCur (make-aB dS))
  (define aBNxt (make-aB dS))

;  (displayln (bytes->list aBCur))

  (for ([y (in-naturals)]
        [str (in-list lStr)])
    (for ([x (in-naturals)]
          [ch (in-string str)])
      (define b (if (char=? #\# ch) 1 0))
      ;(display tfn)
      (aB-set! aBCur dS (+ x dSEdge) (+ y dSEdge) sCenter sCenter b)
    )
  )

;  (displayln (bytes->list aBCur))

  (define mpIBLIBAdj (MpIBLIBAdj dS))

  (SDish dS aBCur aBNxt mpIBLIBAdj))

(define-inline (CBAlive aB dS mpIBLIBAdj x y z w)
  (let ([liBAdj (unsafe-vector-ref mpIBLIBAdj (make-iB dS x y z w))])
    (count (lambda (iBAdj) (unsafe-fx= (aB-raw-ref aB iBAdj) 1)) liBAdj)))

(define (LmXyzw aB dS)
    (define xyzwFirst (list dS dS dS dS))
    (define xyzwLast (list 0 0 0 0))

    (define (XyzwMin xyzw1 xyzw2)
        (list
            (min (first xyzw1) (first xyzw2))
            (min (second xyzw1) (second xyzw2))
            (min (third xyzw1) (third xyzw2))
            (min (fourth xyzw1) (fourth xyzw2))))
    (define (XyzwMax xyzw1 xyzw2)
        (list
            (max (first xyzw1) (first xyzw2))
            (max (second xyzw1) (second xyzw2))
            (max (third xyzw1) (third xyzw2))
            (max (fourth xyzw1) (fourth xyzw2))))

    (for* ([x (in-range dS)] [y (in-range dS)] [z (in-range dS)] [w (in-range dS)])
        (define fOn (aB-ref aB dS x y z w))
        (when fOn
            (define xyzwCur (list x y z w))
            (set! xyzwFirst (XyzwMin xyzwFirst xyzwCur))
            (set! xyzwLast (XyzwMax xyzwLast xyzwCur))

            ; (displayln (list xyzwCur xyzwFirst xyzwLast))
        )
    )

    (cons xyzwFirst xyzwLast)
)

(define (dish-tick dish fDebug)
  (match-define (SDish dS aBCur aBNxt mpIBLIBAdj) dish)
  (when fDebug
    (displayln (bytes->list aBCur))
    (newline)
  )
  (for* ([w (in-range dS)] [z (in-range dS)] [y (in-range dS)] [x (in-range dS)])
    (define fOld (aB-ref aBCur dS x y z w))
    (define cBAlive (CBAlive aBCur dS mpIBLIBAdj x y z w))
    (define fNew
      (if fOld
        (if (or (unsafe-fx= cBAlive 2) (unsafe-fx= cBAlive 3)) 1 0)
        (if (unsafe-fx= cBAlive 3) 1 0)))
    (when (and fDebug fOld)
        (define iB (make-iB dS x y z w))
        (define lIBAdj (unsafe-vector-ref mpIBLIBAdj iB))
        (displayln (~a x " " y " " z " " w))
        (displayln (~a dS " " (* dS dS)))
        (displayln iB)
        ;(displayln fOld)
        (displayln (length lIBAdj))
        (displayln lIBAdj)
        (displayln cBAlive)
        (displayln fNew)
        (newline)
        ; (when (unsafe-fx>= z 5)
        ;     (exit 1)
        ; )
    )
    (aB-set! aBNxt dS x y z w fNew))
  (set-SDish-aBCur! dish aBNxt)
  (set-SDish-aBNxt! dish aBCur))

(define (dish-display dish)
 (match-define (SDish dS aBCur aBNxt mpIBLIBAdj) dish)
 (define lmXyzw (LmXyzw aBCur dS))
;  (displayln lmXyzw)
;  (displayln (bytes->list aBCur))
 (define xyzwMin (car lmXyzw))
 (define xyzwMax (cdr lmXyzw))

 (for ([w (in-range (fourth xyzwMin) (add1 (fourth xyzwMax)))])
    (for ([z (in-range (third xyzwMin) (add1 (third xyzwMax)))])
        (for ([y (in-range (second xyzwMin) (add1 (second xyzwMax)))])
            (displayln 
                (list->string
                    (for/list ([x (in-range (first xyzwMin) (add1 (first xyzwMax)))])
                        (if (aB-ref aBCur dS x y z w) #\# #\.)))))
        (newline)
    )
    (displayln "+++")
  )
 (newline)
 (displayln "---")
 (newline)
)

;(displayln g_lStr)
(define g_dish (lstring->dish g_lStr))

(dish-display g_dish)

(for ([i (in-range 6)])
    (dish-tick g_dish #f)
;    (dish-display g_dish)
)

(begin
 (match-define (SDish dS aBCur _ _) g_dish)
 (define lB (bytes->list aBCur))
 (count (lambda (b) (= b 1)) lB)
)
