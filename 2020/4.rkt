#lang racket

(require mzlib/etc)
(define g_pathCode (this-expression-source-directory))

(require srfi/1) ; for drop-right
(require srfi/13) ; for string-take etc

(require racket/trace)

; day 3

(define g_pathData
    (build-path
        g_pathCode
        "4c.txt"))
(define g_pathOut
    (build-path
        g_pathCode
        "4ro.txt"))

(define strFile (string-join (string-split (file->string g_pathData) "\r") ""))
(define lStrFile (string-split strFile "\n\n"))
(define lStrPass (map (lambda (strFile) (string-join (string-split strFile) " ")) lStrFile))

; (define g_lStrLine (map string-trim (file->lines g_pathData)))

; (define (LStrPassFromLStrLine i_lStrLine)
;     (let go ([lStrLine i_lStrLine] [lStrPass '()] [lStrCur '()])
;         (define lStrJoined (if (empty? lStrCur) '() (list (string-join lStrCur " "))))
;         (cond
;             [(empty? lStrLine) (append lStrPass lStrJoined)]
;             [(equal? 0 (string-length (first lStrLine)))
;                 (go (rest lStrLine) (append lStrPass lStrJoined) '())
;             ]
;             [else
;                 (go (rest lStrLine) lStrPass (append lStrCur (list (first lStrLine))))
;             ])))

; (define lStrPass (LStrPassFromLStrLine g_lStrLine))

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
; (define lPass (map PassFromStr g_lStrLine))

(define (pass-valid? pass)
    (define setKey (list->set (hash-keys pass)))
    (or
        (equal? setKey setKeyMost)
        (equal? setKey setKeyAll)
    )
)

(define (pass-year-legit? pass strKey yearFirst yearLast)
    (define strVal (hash-ref pass strKey '()))
    (if (string? strVal)
        (let ()
            (define pat #px"^[0-9]{4}$")
            (define lStrMatch (regexp-match pat strVal))
            (if (list? lStrMatch)
                (let ()
                    (define year (string->number (first lStrMatch)))
                    (and (>= year yearFirst) (<= year yearLast))
                )
                #f
            )
        )
        #f
    )
)

(define (pass-height-legit? pass strKey cmFirst cmLast inFirst inLast)
    (define strVal (hash-ref pass strKey '()))
    (if (string? strVal)
        (let ()
            (define pat #px"^([0-9]+)(in|cm)$")
            (define lStrMatch (regexp-match pat strVal))
            (if (list? lStrMatch)
                (let ()
                    (match-define (list strAll strNum strUnit) lStrMatch)
                    (define num (string->number strNum))
                    (cond
                        [(equal? strUnit "cm") (and (>= num cmFirst) (<= num cmLast))]
                        [(equal? strUnit "in") (and (>= num inFirst) (<= num inLast))]
                        [else #f]
                    )
                )
                #f
            )
        )
        #f
    )
)

(define (pass-regexp-legit? pass strKey pat)
    (define strVal (hash-ref pass strKey '()))
    (and
        (string? strVal)
        (regexp-match? pat strVal)
    )
)

(define (pass-byr-legit? pass)
    (pass-year-legit? pass "byr" 1920 2002)
)

(define (pass-iyr-legit? pass)
    (pass-year-legit? pass "iyr" 2010 2020)
)

(define (pass-eyr-legit? pass)
    (pass-year-legit? pass "eyr" 2020 2030)
)

(define (pass-hgt-legit? pass)
    (pass-height-legit? pass "hgt" 150 193 59 76)
)

(define (pass-hcl-legit? pass)
    (pass-regexp-legit? pass "hcl" #px"^#[0-9a-f]{6}$")
)

(define (pass-ecl-legit? pass)
    (pass-regexp-legit? pass "ecl" #px"^(amb|blu|brn|gry|grn|hzl|oth)$")
)

(define (pass-pid-legit? pass)
    (pass-regexp-legit? pass "pid" #px"^[0-9]{9}$")
)

(define (pass-legit? pass)
    (and
        (pass-iyr-legit? pass)
        (pass-byr-legit? pass)
        (pass-eyr-legit? pass)
        (pass-hgt-legit? pass)
        (pass-hcl-legit? pass)
        (pass-ecl-legit? pass)
        (pass-pid-legit? pass)
    )
)

(define (str-pass-legit? strPass)
    (pass-legit? (PassFromStr strPass))
)

;(list->set (hash-keys (first lPass)))
; (define lPass8 (filter (lambda (pass) (equal? 8 (length (hash-keys pass)))) lPass))
; (list->set (hash-keys (first lPass8)))
; setKeyAll

(define lPassValid (filter pass-valid? lPass))
(define lPassLegit (filter pass-legit? lPass))
(define lStrPassLegit (filter str-pass-legit? lStrPass))

(length lPass)
(length lPassValid)
(length lPassLegit)
(length lStrPassLegit)

;(display-lines-to-file lStrPassLegit g_pathOut)

;(pass-legit? (PassFromStr "eyr:2027 iyr:2017 hgt:160in byr:1990 pid:131099122 hcl:#623a2f ecl:amb"))

