#lang racket/base

; from https://github.com/jeapostrophe/exp/blob/master/life.rkt

(require racket/match
         racket/unsafe/ops
         racket/performance-hint
         racket/vector)

(struct SDish (dX dY aBCur aBNxt mpIBLIBAdj) #:mutable)

(define (make-aB dX dY) (make-bytes (* dX dY)))
(define (make-iB dX x y) (unsafe-fx+ x (unsafe-fx* dX y)))
(define-inline (aB-set! aB dX x y ?)
  (unsafe-bytes-set! aB (make-iB dX x y) (if ? 1 0)))
(define-inline (aB-ref aB dX x y)
  (unsafe-fx= 1 (unsafe-bytes-ref aB (make-iB dX x y))))
(define-inline (aB-raw-ref aB iB)
  (unsafe-fx= 1 (unsafe-bytes-ref aB iB)))

(define (MpIBLIBAdj dX dY)
    (define lXyAll (for*/list ([x (in-range -1 2)] [y (in-range -1 2)]) (cons x y)))
    (define lXyAdj (filter (lambda (xy) (or (not (zero? (car xy))) (not (zero? (cdr xy))))) lXyAll))
    (define (FXyOk xy)
      (define x (car xy))
      (define y (cdr xy))
      (and (>= x 0) (< x dX) (>= y 0) (< y dY)))
    (for*/vector ([x (in-range dX)] [y (in-range dY)])
      (define lXyNear (map (lambda (xy) (cons (+ x (car xy)) (+ y (cdr xy)))) lXyAdj))
      (define lXyOk (filter FXyOk lXyNear))
      (define (IbFromXy xy) (make-iB dX (car xy) (cdr xy)))
      (list->vector (map IbFromXy lXyOk))))

(define (string->dish str)
  (local-require racket/string)
  (define lStr (string-split str))
  (define dX
    (* 1 (length lStr)))
  (define dY
    (* 1 (apply max (map string-length lStr))))
  (define aBCur (make-aB dX dY))
  (define aBNxt (make-aB dX dY))

  (for ([x (in-naturals)]
        [str (in-list lStr)])
    (for ([y (in-naturals)]
          [ch (in-string str)])
      (aB-set! aBCur dX x y (char=? #\O ch))))

  (define mpIBLIBAdj (MpIBLIBAdj dX dY))

  (SDish dX dY aBCur aBNxt mpIBLIBAdj))

(define (tick-alt dish)
  (match-define (SDish dX dY aBCur aBNxt mpIBLIBAdj) dish)
  (displayln (vector-take-right mpIBLIBAdj 3)))

(define-inline (CBAlive2 aB dX dY mpIBLIBAdj x y)
  (let ([cBAlive 0])
    (for ([iBNear (unsafe-vector-ref mpIBLIBAdj (make-iB dX x y))])
      (when (aB-raw-ref aB iBNear)
        (set! cBAlive (unsafe-fx+ 1 cBAlive))))
    cBAlive))

(define-syntax-rule (unsafe-between min x max)
  (and (unsafe-fx<= min x)
       (unsafe-fx< x max)))
(define-inline (CBAlive aB dX dY mpIBLIBAdj x y)
  (let ([cBAlive 0])
    (for ([dXNear (in-range -1 +2)])
      (let ([xNear (unsafe-fx+ dXNear x)])
        (when (unsafe-between 0 xNear dX)
          (for ([dYNear (in-range -1 +2)])
            (unless (and (unsafe-fx= dXNear 0) (unsafe-fx= dYNear 0))
              (let ([yNear (unsafe-fx+ dYNear y)])
                (when (and (unsafe-between 0 yNear dY)
                           (aB-ref aB dX xNear yNear))
                  (set! cBAlive (unsafe-fx+ 1 cBAlive)))))))))
    cBAlive))

(define (tick dish fnCBAlive)
  (match-define (SDish dX dY aBCur aBNxt mpIBLIBAdj) dish)
  (for* ([x (in-range dX)]
         [y (in-range dY)])
    (define alive? (aB-ref aBCur dX x y))
    (define cBAlive (fnCBAlive aBCur dX dY mpIBLIBAdj x y))
    (define new-alive?
      (or (and alive? (or (unsafe-fx= cBAlive 2) (unsafe-fx= cBAlive 3)))
          (and (not alive?) (unsafe-fx= cBAlive 3))))
    (aB-set! aBNxt dX x y new-alive?))
  (set-SDish-aBCur! dish aBNxt)
  (set-SDish-aBNxt! dish aBCur)
  dish)

(module+ test-bench
  ;;      original: cpu time: 1843 real time: 1842 gc time: 36
  ;; aB-ref/set: cpu time: 1683 real time: 1682 gc time: 82
  ;;     neighbors: cpu time:  530 real time:  531 gc time: 0
  (define (let-there-be-life str)
    (define seed (string->dish str))
    (collect-garbage)
    (collect-garbage)
    (time
     (for ([i (in-range 10000)])
       (tick seed CBAlive2)))))

(module+ test
  (require 2htdp/universe
           2htdp/image)

  (define (draw dish)
    (match-define (SDish dX dY aBCur _ _) dish)
    (define SCALE 10)
    (define BOX
      (square SCALE "solid" "black"))
    (for*/fold ([img (empty-scene
                      (* SCALE dY)
                      (* SCALE dX))])
        ([x (in-range dX)]
         [y (in-range dY)])
      (if (aB-ref aBCur dX x y)
        (place-image BOX 
                     (+ (/ SCALE 2) 0.5 (* y SCALE))
                     (+ (/ SCALE 2) 0.5 (* x SCALE))
                     img)
        img)))

  (define (let-there-be-life str)
    (big-bang (string->dish str)
              [on-tick tick]
              [on-draw draw])))

(module+ test-bench
  (let-there-be-life
   "........................O...........
    ......................O.O...........
    ............OO......OO............OO
    ...........O...O....OO............OO
    OO........O.....O...OO..............
    OO........O...O.OO....O.O...........
    ..........O.....O.......O...........
    ...........O...O....................
    ............OO......................"))

;; xxx run on GPU?
;; xxx use bit packing on a row?