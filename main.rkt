#lang racket

; There is a racket program(boring)

(define (bored-game)
  (let* ([x (random 0 10)]
         [o (random 0 10)]
         [diff (abs (- x o))])
    (cond
      [(x . > . o) (printf "x win: ~a" diff)]
      [(o . > . x) (printf "o win: ~a" diff)]
      [else (printf "x and o both win.")])))

(bored-game)
