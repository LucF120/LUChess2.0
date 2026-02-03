#lang racket

(require "squares.rkt"
         "pieces.rkt")

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

(define (print-board board)
  (for [(r (in-range 7 -1 -1))
        (rankstr '(8 7 6 5 4 3 2 1))]
    (display (string-append (number->string rankstr) "| "))
    (let [(rank (vector-ref board r))]
      (for [(f (in-range 8))]
        (let* [(p (vector-ref rank f))
               (unicode (piece->unicode p))]
          (display (string-append unicode " "))))
      (display "\n")))
  (display "   a b c d e f g h"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  Actual Game Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: This doesnt check literally anything lol 
(define (move board from to)
  (if (legal-move? board from to)
      (let [(p (get-square board from))]
        (set-square board from 0)
        (set-square board to p))
      (error 'invalid-move "")))

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
      
; Check if any pieces obstruct the view from 1 square to another
(define (can-see-square? board from to)
  (if (equal? from to)
      #f  ; Pieces cant move to themselves 
      (let* [(from-coord (square->idx from))
            (to-coord (square->idx to))
            (from-rank (coordinate-rank from-coord))
            (from-file (coordinate-file from-coord))
            (to-rank (coordinate-rank to-coord))
            (to-file (coordinate-file to-coord))
            (horizontal (equal? from-rank to-rank))
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
                        (in-range-auto from-rank to-rank)))]))))
  
;;; Note: The board is not needed to check if a knight move is legal
;;; simply because knights can jump over pieces.
;;; - This is not the case for any other piece 
(define (legal-knight-move? from to)
  (let* [(from-coord (square->idx from))
         (from-file-idx (coordinate-file from-coord))
         (from-rank-idx (coordinate-rank from-coord))
         (to-coord (square->idx to))
         (to-file-idx (coordinate-file to-coord))
         (to-rank-idx (coordinate-rank to-coord))
         (horiz-dist (abs (- from-file-idx to-file-idx)))
         (vert-dist (abs (- from-rank-idx to-rank-idx)))]
    ; Knights must move 2 vert and 1 horizontal,
    ; or 1 vert and 2 horizontal 
    (if (or (and (equal? horiz-dist 2) (equal? vert-dist 1))
            (and (equal? horiz-dist 1) (equal? vert-dist 2)))
        #t
        #f)))

(define (legal-pawn-move? board p from to)
  (let* [(from-coord (square->idx from))
         (from-file-idx (coordinate-file from-coord))
         (from-rank-idx (coordinate-rank from-coord))
         (to-coord (square->idx to))
         (to-file-idx (coordinate-file to-coord))
         (to-rank-idx (coordinate-rank to-coord))
         (horiz-dist (- to-file-idx from-file-idx))
         (vert-dist (- to-rank-idx from-rank-idx))]
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
      [(and (equal? vert-dist 1)
            (equal? horiz-dist 1))
       (piece? (get-square board to))]
      
      ; One move forward case. Only legal if there is not a piece
      ; on that square.
      [(and (equal? vert-dist 1)
            (equal? horiz-dist 0))
       (not (piece? (get-square board to)))]
      
      [else #f])))


(define (legal-move? board from to)
  (cond
    ; Trying to move an invisible piece 
    [(empty-square? board from) #f]

    ; Can't see the square if one of the following is true:
    ;  1. Not on the same vertical, horizontal, or diagonal line
    ;  2. There is a piece in the way on that line 
    [(not (can-see-square? board from to)) #f]
    [else 
     (if (empty-square? board from)
         #f  ; Trying to move an invisible piece 
         (let* [(p-from (get-square board from))
                (p-to (get-square board to))]
           (if (and (piece? p-to)
                    (equal? (piece-color p-from) (piece-color p-to)))
               #f ; Trying to capture your own piece 
               (cond
                 [(knight? p-from) (legal-knight-move? from to)]
                 [(pawn? p-from) (legal-pawn-move? board p-from
                                                   from to)]
                 [else #f #;(error 'impl "")]))))]))
    
