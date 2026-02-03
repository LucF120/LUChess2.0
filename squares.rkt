#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         Rank Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define a 'a)
(define b 'b)
(define c 'c)
(define d 'd)
(define e 'e)
(define f 'f)
(define g 'g)
(define h 'h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           Squares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define a1 'a1)
(define a2 'a2)
(define a3 'a3)
(define a4 'a4)
(define a5 'a5)
(define a6 'a6)
(define a7 'a7)
(define a8 'a8)

(define b1 'b1)
(define b2 'b2)
(define b3 'b3)
(define b4 'b4)
(define b5 'b5)
(define b6 'b6)
(define b7 'b7)
(define b8 'b8)

(define c1 'c1)
(define c2 'c2)
(define c3 'c3)
(define c4 'c4)
(define c5 'c5)
(define c6 'c6)
(define c7 'c7)
(define c8 'c8)

(define d1 'd1)
(define d2 'd2)
(define d3 'd3)
(define d4 'd4)
(define d5 'd5)
(define d6 'd6)
(define d7 'd7)
(define d8 'd8)

(define e1 'e1)
(define e2 'e2)
(define e3 'e3)
(define e4 'e4)
(define e5 'e5)
(define e6 'e6)
(define e7 'e7)
(define e8 'e8)

(define f1 'f1)
(define f2 'f2)
(define f3 'f3)
(define f4 'f4)
(define f5 'f5)
(define f6 'f6)
(define f7 'f7)
(define f8 'f8)

(define g1 'g1)
(define g2 'g2)
(define g3 'g3)
(define g4 'g4)
(define g5 'g5)
(define g6 'g6)
(define g7 'g7)
(define g8 'g8)

(define h1 'h1)
(define h2 'h2)
(define h3 'h3)
(define h4 'h4)
(define h5 'h5)
(define h6 'h6)
(define h7 'h7)
(define h8 'h8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for converting square names to 2D vector coordinates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a8 = (coordinate 0 7)
; c4 = (coordinate 2 3)
(struct coordinate (file rank))

; Returns what a rank corresponds to in the board 2D vector
; Example: rank 1 is idx 0 
(define (rank->idx rank)
  (if (or (< rank 1) (> rank 8))
      (error 'invalid-rank rank)
      (- rank 1)))

(define (idx->rank idx)
  (if (or (< idx 0) (> idx 7))
      (error 'invalid-idx-rank idx)
      (+ idx 1)))

; Returns the idx of a file in the board 2D vector
(define (file->idx file)
  (cond
    [(equal? file a) 0]
    [(equal? file b) 1]
    [(equal? file c) 2]
    [(equal? file d) 3]
    [(equal? file e) 4]
    [(equal? file f) 5]
    [(equal? file g) 6]
    [(equal? file h) 7]
    [else (error 'invalid-file file)]))


; Returns the idx of a file in the board 2D vector
(define (idx->file idx)
  (cond
    [(equal? idx 0) a]
    [(equal? idx 1) b]
    [(equal? idx 2) c]
    [(equal? idx 3) d]
    [(equal? idx 4) e]
    [(equal? idx 5) f]
    [(equal? idx 6) g]
    [(equal? idx 7) h]
    [else (error 'invalid-file-idx idx)]))

; Returns the file of a square
; Example: (get-file 'a1) -> a 
(define (get-file sqr)
  (string->symbol (substring (symbol->string sqr) 0 1)))

; Returns the rank of a square
; Example: (get-rank 'a1) -> 1
(define (get-rank sqr)
  (string->number (substring (symbol->string sqr) 1 2)))

; Returns the coordintes in the vector for a square
; Example: c4 ->
; c => 2
; 4 => 3
; -------------> (coordinate 3 2)
(define (square->idx sqr)
  (let [(file (get-file sqr))
        (rank (get-rank sqr))]
    (coordinate (file->idx file)
                (rank->idx rank))))

; The reverse of square->idx. Convert coordinate back to a symbol
; Example: (coordinate 3 2) -> 'c4 
(define (idx->square coord)
  (let [(file (idx->file (coordinate-file coord)))
        (rank (idx->rank (coordinate-rank coord)))]
    (string->symbol (string-append (symbol->string file)
                                   (number->string rank)))))
