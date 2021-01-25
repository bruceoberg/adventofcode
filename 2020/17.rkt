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

(define (make-aB dS) (make-bytes (expt dS 3)))
(define (make-iB dS x y z) (unsafe-fx+ (unsafe-fx* (unsafe-fx+ (unsafe-fx* z dS) y) dS) x))
(define-inline (aB-set! aB dS x y z b)
  (unsafe-bytes-set! aB (make-iB dS x y z) b))
(define-inline (aB-ref aB dS x y z)
  (unsafe-fx= 1 (unsafe-bytes-ref aB (make-iB dS x y z))))
(define-inline (aB-raw-ref aB iB)
  (unsafe-bytes-ref aB iB))

(define (MpIBLIBAdj dS)
    (define lXyzAll (for*/list ([x (in-range -1 2)] [y (in-range -1 2)] [z (in-range -1 2)]) (list x y z)))
    (define lXyzAdj (filter (lambda (xyz) (or (not (zero? (first xyz))) (not (zero? (second xyz))) (not (zero? (third xyz))))) lXyzAll))
    (define (FXyzOk xyz)
      (define x (first xyz))
      (define y (second xyz))
      (define z (third xyz))
      (and (>= x 0) (< x dS) (>= y 0) (< y dS) (>= z 0) (< z dS)))
    (define (IbFromXyz xyz) (make-iB dS (first xyz) (second xyz) (third xyz)))
    (define (XyzAdd xyz1 xyz2) (list (+ (first xyz1) (first xyz2)) (+ (second xyz1) (second xyz2)) (+ (third xyz1) (third xyz2))))
    (for*/vector ([z (in-range dS)] [y (in-range dS)] [x (in-range dS)])
      (define xyzCur (list x y z))
      (define lXyzNear (map (lambda (xyzAdj) (XyzAdd xyzAdj xyzCur)) lXyzAdj))
      (define lXyzOk (filter FXyzOk lXyzNear))
      (define (IbFromXyz xyz) (make-iB dS (first xyz) (second xyz) (third xyz)))
      (sort (map IbFromXyz lXyzOk) <)))

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
      (aB-set! aBCur dS (+ x dSEdge) (+ y dSEdge) sCenter b)
    )
  )

;  (displayln (bytes->list aBCur))

  (define mpIBLIBAdj (MpIBLIBAdj dS))

  (SDish dS aBCur aBNxt mpIBLIBAdj))

(define-inline (CBAlive aB dS mpIBLIBAdj x y z)
  (let ([liBAdj (unsafe-vector-ref mpIBLIBAdj (make-iB dS x y z))])
    (count (lambda (iBAdj) (unsafe-fx= (aB-raw-ref aB iBAdj) 1)) liBAdj)))

(define (LmXyz aB dS)
    (define xyzFirst (list dS dS dS))
    (define xyzLast (list 0 0 0))

    (define (XyzMin xyz1 xyz2) (list (min (first xyz1) (first xyz2)) (min (second xyz1) (second xyz2)) (min (third xyz1) (third xyz2))))
    (define (XyzMax xyz1 xyz2) (list (max (first xyz1) (first xyz2)) (max (second xyz1) (second xyz2)) (max (third xyz1) (third xyz2))))

    (for* ([x (in-range dS)] [y (in-range dS)] [z (in-range dS)])
        (define xyzCur (list x y z))
        (define fOn (aB-ref aB dS x y z))
        (when fOn
            (set! xyzFirst (XyzMin xyzFirst xyzCur))
            (set! xyzLast (XyzMax xyzLast xyzCur))

            ; (displayln (list xyzCur xyzFirst xyzLast))
        )
    )

    (cons xyzFirst xyzLast)
)

(define (dish-tick dish fDebug)
  (match-define (SDish dS aBCur aBNxt mpIBLIBAdj) dish)
  (when fDebug
    (displayln (bytes->list aBCur))
    (newline)
  )
  (for* ([z (in-range dS)] [y (in-range dS)] [x (in-range dS)])
    (define fOld (aB-ref aBCur dS x y z))
    (define cBAlive (CBAlive aBCur dS mpIBLIBAdj x y z))
    (define fNew
      (if fOld
        (if (or (unsafe-fx= cBAlive 2) (unsafe-fx= cBAlive 3)) 1 0)
        (if (unsafe-fx= cBAlive 3) 1 0)))
    (when (and fDebug fOld)
        (define iB (make-iB dS x y z))
        (define lIBAdj (unsafe-vector-ref mpIBLIBAdj iB))
        (displayln (~a x " " y " " z))
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
    (aB-set! aBNxt dS x y z fNew))
  (set-SDish-aBCur! dish aBNxt)
  (set-SDish-aBNxt! dish aBCur))

(define (dish-display dish)
 (match-define (SDish dS aBCur aBNxt mpIBLIBAdj) dish)
 (define lmXyz (LmXyz aBCur dS))
;  (displayln lmXyz)
;  (displayln (bytes->list aBCur))
 (define xyzMin (car lmXyz))
 (define xyzMax (cdr lmXyz))

 (for ([z (in-range (third xyzMin) (add1 (third xyzMax)))])
    (for ([y (in-range (second xyzMin) (add1 (second xyzMax)))])
        (displayln 
            (list->string
                (for/list ([x (in-range (first xyzMin) (add1 (first xyzMax)))])
                    (if (aB-ref aBCur dS x y z) #\# #\.)))))
    (newline)
 )
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
