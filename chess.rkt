#lang racket

(require "squares.rkt"
         "pieces.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct game (board white-king-square black-king-square))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Creating, updating, and printing board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update a square of the board to contain the given piece
(define (set-square board sqr piece)
  (let* [(coord (square->idx sqr))
         (file-idx (coordinate-file coord))
         (rank-idx (coordinate-rank coord))
         (rank (vector-ref board rank-idx))]
    (vector-set! rank file-idx piece)))

;;; Get the piece that is at a given square 
(define (get-square board sqr)
  (let* [(coord (square->idx sqr))
         (file-idx (coordinate-file coord))
         (rank-idx (coordinate-rank coord))
         (rank (vector-ref board rank-idx))]
    (vector-ref rank file-idx)))

(define (init-board)
  ;;; Set an entire rank to a vector of pieces
  ;;; This is only useful when initializing the board
  (define (set-rank board rank pieces)
    (vector-set! board
                 (rank->idx rank)
                 pieces))

  (let [(board (make-vector 8 0))]
    ;;; Ranks with actual pieces 
    (set-rank board 1 (list->vector init-rank1))
    (set-rank board 2 (list->vector init-rank2))
    (set-rank board 7 (list->vector init-rank7))
    (set-rank board 8 (list->vector init-rank8))
    
    ;;; Empty ranks.
    (set-rank board 3 (make-vector 8 0))
    (set-rank board 4 (make-vector 8 0))
    (set-rank board 5 (make-vector 8 0))
    (set-rank board 6 (make-vector 8 0))
    board))

(define (init-game)
  (let [(board (init-board))
        (white-king-loc e1)
        (black-king-loc e8)]    
    (game board white-king-loc black-king-loc)))

(define (print-board g)
  (let [(board (game-board g))]
    (for [(r (in-range 7 -1 -1))
          (rankstr '(8 7 6 5 4 3 2 1))]
      (display (string-append (number->string rankstr) "| "))
      (let [(rank (vector-ref board r))]
        (for [(f (in-range 8))]
          (let* [(p (vector-ref rank f))
                 (unicode (piece->unicode p))]
            (display (string-append unicode " "))))
        (display "\n")))
    (display "   a b c d e f g h")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  Actual Game Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move g from to)
  (let [(board (game-board g))]
    (if (legal-move? g from to)
        (let [(p (get-square board from))]
          (set-square board from 0)
          (set-square board to p))
        (error 'invalid-move ""))))

;;; Check if the sqr has a piece on it 
(define (empty-square? board sqr)
  (if (equal? (get-square board sqr) 0)
      #t
      #f))

(define (diagonal? from-rank from-file to-rank to-file)
  (let [(dx (abs (- from-file to-file)))
        (dy (abs (- from-rank to-rank)))]
    (and (equal? dx dy) (not (equal? dx 0)))))

; Helper function that automatically determines whether the range
; should increment up or down
; It will also only output the numbers BETWEEN the start and end
;  for convenience in the can-see-square? function 
; (in-range-auto 0 4) -> '(1 2 3)
; (in-range-auto 4 0) -> '(3 2 1)
;
(define (in-range-auto from to)
  (stream->list (if (> from to)
                    (in-range (- from 1) to -1)
                    (in-range (+ from 1) to))))

; Helper function that takes two squares, and returns the
; indexes into the 2D vector, and the vert/horiz distances
; Example:
;   (destruct-coords b2 g7) = from-rank = 1
;                             from-file = 1
;                             to-rank   = 6
;                             to-file   = 6
;                             horiz-dist = 5
;                             vert-dist =  5
(define (destruct-coords from to)
  (let* [(from-coord (square->idx from))
         (to-coord (square->idx to))
         (from-rank (coordinate-rank from-coord))
         (from-file (coordinate-file from-coord))
         (to-rank (coordinate-rank to-coord))
         (to-file (coordinate-file to-coord))
         (horiz-dist (- to-file from-file))
         (vert-dist (- to-rank from-rank))]
    (values from-rank
            from-file
            to-rank
            to-file
            horiz-dist
            vert-dist)))

; Check if any pieces obstruct the view from 1 square to another
(define (can-see-square? board from to)
  (if (equal? from to)
      #f  ; Pieces cant move to themselves
      (let-values [((from-rank from-file to-rank to-file horiz-dist vert-dist)
                    (destruct-coords from to))]
        (let [(horizontal (equal? from-rank to-rank))
              (vertical (equal? from-file to-file))
              (diagonal (diagonal? from-rank from-file to-rank to-file))]
          (cond
            [horizontal
             (andmap (lambda (x)
                       (not (piece? (get-square board
                                                (idx->square (coordinate x from-rank))))))
                     (in-range-auto from-file to-file))]
            [vertical
             (andmap (lambda (x)
                       (not (piece? (get-square board
                                                (idx->square (coordinate from-file x))))))
                     (in-range-auto from-rank to-rank))]
            [diagonal
             (andmap (lambda (x)
                       (not (piece? (get-square board
                                                (idx->square (coordinate (car x) (cadr x)))))))
                     (map list
                          (in-range-auto from-file to-file)
                          (in-range-auto from-rank to-rank)))]
            [else #f])))))


(define (legal-knight-move? from to)
  (let-values [((from-rank from-file to-rank to-file horiz-dist vert-dist)
                (destruct-coords from to))]
    ; Knights must move 2 vert and 1 horizontal,
    ; or 1 vert and 2 horizontal 
    (if (or (and (equal? (abs horiz-dist) 2) (equal? (abs vert-dist) 1))
            (and (equal? (abs horiz-dist) 1) (equal? (abs vert-dist) 2)))
        #t
        #f)))

(define (legal-pawn-move? board p from to)
  (let-values [((from-rank from-file to-rank to-file horiz-dist vert-dist)
                (destruct-coords from to))]
    (cond
      ; White pawn going down or black pawn going up
      [(or (and (< vert-dist 0) (white? p))
           (and (> vert-dist 0) (black? p)))
       #f]
      
      ; Double jump case. Only legal on first move. 
      [(and (equal? (abs vert-dist) 2)
            (equal? horiz-dist 0))
       (or (and (white? p)
                (equal? (get-rank from) 2))
           (and (black? p)
                (equal? (get-rank from) 7)))]
      
      ; Capture case. Only legal if there is a piece to capture
      ; TODO: implement en passant
      [(and (equal? (abs vert-dist) 1)
            (equal? (abs horiz-dist) 1))
       (piece? (get-square board to))]
      
      ; One move forward case. Only legal if there is not a piece
      ; on that square.
      [(and (equal? (abs vert-dist) 1)
            (equal? horiz-dist 0))
       (not (piece? (get-square board to)))]
      
      [else #f])))

; A rook move is legal if the from and to are the same rank, or the same file 
(define (legal-rook-move? from to)
  (let-values [((from-rank from-file to-rank to-file horiz-dist vert-dist)
                (destruct-coords from to))]
    (or (equal? from-rank to-rank)
        (equal? from-file to-file))))

; A bishop move is legal if it is moving diagonally
(define (legal-bishop-move? from to)
  (let-values [((from-rank from-file to-rank to-file horiz-dist vert-dist)
                (destruct-coords from to))]
    (diagonal? from-rank from-file to-rank to-file)))

; A queen move is legal if it is moving vertically, horizontally, or diagonally
(define (legal-queen-move? from to)
  (let-values [((from-rank from-file to-rank to-file horiz-dist vert-dist)
                (destruct-coords from to))]
    (or (equal? from-rank to-rank)
        (equal? from-file to-file)
        (diagonal? from-rank from-file to-rank to-file))))

; A king move is legal if it is moving 1 square in any direction, and is not
; moving itself into danger
;
; Another legal king move is e1-c1 and e1-g1 when castling. This is allowed if
;    1. The king is not in check
;    2. The king and rook have never moved
;    3. The king doesnt castle through check
;          When long castling, c1 and d1 cant be under attack. 
;          When short castling, f1 cant be under attack. 
;
(define (legal-king-move? from to)
  (error 'impl ""))

(define (can-see-white-king? g sqr)
  (let [(board (game-board g))
        (white-king-sqr (game-white-king-square g))]
    (can-see-square? board sqr white-king-sqr)))

(define (can-see-black-king? g sqr)
  (let [(board (game-board g))
        (black-king-sqr (game-black-king-square g))]
    (can-see-square? board sqr black-king-sqr)))

(define (can-see-my-king? g sqr)
  (let* [(board (game-board g))
        (p (get-square board sqr))]
    (if (white? p)
        (can-see-white-king? g sqr)
        (can-see-black-king? g sqr))))

(define (can-see-opposing-king? g sqr)
  (let* [(board (game-board g))
        (p (get-square board sqr))]
    (if (white? p)
        (can-see-black-king? g sqr)
        (can-see-white-king? g sqr))))

(define (legal-move? g from to)
  (let* [(board (game-board g))
        (p-from (get-square board from))
        (p-to (get-square board to))]
    (cond
      ; BAD: Trying to move an invisible piece 
      [(empty-square? board from)
       #f]
      
      ; BAD: Can't see the destination square.
      ;  This occurs if piece is not a knight, AND...
      ;     - It is not on the same vertical, horizontal, or diagonal line
      ;     - OR, there is a piece in the way on that line 
      [(and (not (knight? p-from))
            (not (can-see-square? board from to (knight? p-from))))
       #f]

      ; BAD: Trying to capture your own piece
      [(and (piece? p-to)
            (equal? (piece-color p-from) (piece-color p-to)))
       #f]

      ; BAD: Move gets your king killed
      ; This only needs to be checked if the piece can SEE the king square!
      ; Idea for checking moving-exposes-king?
      ;       - get all the squares of pieces that can see from
      ;       - using this, get all the pieces that can see from
      ;       - then, see if any of those pieces can see white-king if from is gone 
      #;[(and (can-see-my-king? g from)
            (moving-exposes-king? g from))
            (error 'impl "") ]

      [(rook? p-from) (legal-rook-move? from to)]
      [(knight? p-from) (legal-knight-move? from to)]
      [(bishop? p-from) (legal-bishop-move? from to)]
      [(queen? p-from) (legal-queen-move? from to)]
      [(pawn? p-from) (legal-pawn-move? board p-from from to)]
      [(king? p-from) (legal-king-move? board p-from p-to)]
      [else #f #;(error 'impl "")])))

