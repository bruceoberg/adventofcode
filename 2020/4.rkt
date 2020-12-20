#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right

(require racket/trace)

; day 3

(define g_pathData
    (build-path
        g_pathCode
        "4.txt"))

(define g_lStrLine (map string-trim (file->lines g_pathData)))

(define (LStrPassFromLStrLine i_lStrLine)
    (let go ([lStrLine i_lStrLine] [lStrPass '()] [lStrCur '()])
        (define lStrJoined (if (empty? lStrCur) '() (list (string-join lStrCur " "))))
        (cond
            [(empty? lStrLine) (append lStrPass lStrJoined)]
            [(equal? 0 (string-length (first lStrLine)))
                (go (rest lStrLine) (append lStrPass lStrJoined) '())
            ]
            [else
                (go (rest lStrLine) lStrPass (append lStrCur (list (first lStrLine))))
            ])))

(define lStrPass (LStrPassFromLStrLine g_lStrLine))

(define (PairFromStrRec strRec)
    (define lStrPair (string-split strRec ":"))
    (cons (first lStrPair) (last lStrPair))
)

(define (PassFromStr str)
    (define lStrRec (string-split str))
    (define lPair (map PairFromStrRec lStrRec))
    (make-hash lPair)
)

(define setKeyMost (list->set '(
    "byr"
    "iyr"
    "eyr"
    "hgt"
    "hcl"
    "ecl"
    "pid")))

(define setKeyAll (set-add setKeyMost "cid"))

(define lPass (map PassFromStr lStrPass))

(define (legit-pass? pass)
    (define setKey (list->set (hash-keys pass)))
    (or
        (equal? setKey setKeyMost)
        (equal? setKey setKeyAll)
    )
)


;(list->set (hash-keys (first lPass)))
; (define lPass8 (filter (lambda (pass) (equal? 8 (length (hash-keys pass)))) lPass))
; (list->set (hash-keys (first lPass8)))
; setKeyAll

(define lPassLegit (filter legit-pass? lPass))

(length lPass)
(length lPassLegit)