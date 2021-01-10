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
        "11.txt"))

(define g_lStr (file->lines g_pathData))

; from https://github.com/jeapostrophe/exp/blob/master/life.rkt

(require racket/match
         racket/unsafe/ops
         racket/performance-hint
         racket/vector)

(struct SDish (dX dY aBCur aBNxt mpIBVLIBAdj) #:mutable)

(define (make-aB dX dY) (make-bytes (* dX dY)))
(define (make-iB dY x y) (unsafe-fx+ (unsafe-fx* dY x) y))
(define-inline (aB-set! aB dY x y b)
  (unsafe-bytes-set! aB (make-iB dY x y) b))
(define-inline (aB-ref aB dY x y)
  (unsafe-bytes-ref aB (make-iB dY x y)))
(define-inline (aB-raw-ref aB iB)
  (unsafe-bytes-ref aB iB))

(define (MpIBVLIBAdj dX dY)
    (define lXyAll (for*/list ([x (in-range -1 2)] [y (in-range -1 2)]) (cons x y)))
    (define lXyAdj (filter (lambda (xy) (or (not (zero? (car xy))) (not (zero? (cdr xy))))) lXyAll))
    (define (FXyOk xy)
      (define x (car xy))
      (define y (cdr xy))
      (and (>= x 0) (< x dX) (>= y 0) (< y dY)))
    (define (IbFromXy xy) (make-iB dY (car xy) (cdr xy)))
    (for*/vector ([x (in-range dX)] [y (in-range dY)])
      (for/vector ([xyAdj lXyAdj])
        (define (XyAdd xy1 xy2) (cons (+ (car xy1) (car xy2)) (+ (cdr xy1) (cdr xy2))))
        (let go ([xyCur (cons x y)] [lXyRun '()])
            (define xyNext (XyAdd xyCur xyAdj))
            (cond
                ([FXyOk xyNext] (go xyNext (cons xyNext lXyRun)))
                (else (reverse (map IbFromXy lXyRun))))))))

(define (lstring->dish lStr)
  (define dX (length lStr))
  (define dY (apply max (map string-length lStr)))
  (define aBCur (make-aB dX dY))
  (define aBNxt (make-aB dX dY))

  (for ([x (in-naturals)]
        [str (in-list lStr)])
    (for ([y (in-naturals)]
          [ch (in-string str)])
      (define tfn (cond ([char=? #\L ch] 0) ([char=? #\.] 255) (else 1)))
      ;(display tfn)
      (aB-set! aBCur dY x y tfn)
    )
    ;(displayln ""
  )

  (define mpIBVLIBAdj (MpIBVLIBAdj dX dY))

  (SDish dX dY aBCur aBNxt mpIBVLIBAdj))

(define-inline (CBAlive aB dX dY mpIBVLIBAdj x y)
  (let ([cBAlive 0] [vliBRun (unsafe-vector-ref mpIBVLIBAdj (make-iB dY x y))])
    (for ([liBRun vliBRun])
      (define liBSeat (filter (lambda (iBRun) (not (unsafe-fx= (aB-raw-ref aB iBRun) 255))) liBRun))
      (when (and (not (empty? liBSeat)) (unsafe-fx= (aB-raw-ref aB (car liBSeat)) 1))
        (set! cBAlive (unsafe-fx+ 1 cBAlive))))
    cBAlive))

(define (dish-tick dish fDebug)
  (match-define (SDish dX dY aBCur aBNxt mpIBVLIBAdj) dish)
  (when fDebug
    (displayln (bytes->list aBCur))
    (newline)
  )
  (for* ([x (in-range dX)]
         [y (in-range dY)])
    (define tfnOld (aB-ref aBCur dY x y))
    (define cBAlive (CBAlive aBCur dX dY mpIBVLIBAdj x y))
    (define tfnNew
      (cond
        ([unsafe-fx= tfnOld 0] (if (unsafe-fx= cBAlive 0) 1 0))
        ([unsafe-fx= tfnOld 1] (if (unsafe-fx>= cBAlive 5) 0 1))
        (else 255)))
    (when fDebug
        (define iB (make-iB dY x y))
        (define lIBAdj (unsafe-vector-ref mpIBVLIBAdj iB))
        (displayln iB)
        (displayln tfnOld)
        (displayln lIBAdj)
        (displayln cBAlive)
        (displayln tfnNew)
        (newline)
        (when (unsafe-fx>= y 5)
            (exit 1)
        )
    )
    (aB-set! aBNxt dY x y tfnNew))
  (set-SDish-aBCur! dish aBNxt)
  (set-SDish-aBNxt! dish aBCur)
  (bytes=? aBCur aBNxt))

(define (dish-display dish)
 (match-define (SDish dX dY aBCur aBNxt mpIBVLIBAdj) dish)
 ; (displayln (vector-take mpIBVLIBAdj 2))
 (for ([x (in-range dX)])
    (displayln 
        (list->string
            (for/list ([y (in-range dY)])
                (define tfn (aB-ref aBCur dY x y))
                (cond
                  ([unsafe-fx= tfn 0] #\L)
                    ([unsafe-fx= tfn 1] #\#)
                    (else #\.))))))
  (newline)
)

;(displayln g_lStr)
(define g_dish (lstring->dish g_lStr))

; (dish-display g_dish)
; (dish-tick g_dish #f)
; (dish-display g_dish)
; (dish-tick g_dish #f)
; (dish-display g_dish)

(let go ([dish g_dish])
;    (dish-display dish)
    (if (dish-tick dish #f) (void) (go dish))
)

(begin
 (match-define (SDish dX dY aBCur _ _) g_dish)
 (define lB (bytes->list aBCur))
 (count (lambda (b) (= b 1)) lB)
)
