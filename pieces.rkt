#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct piece (color type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colors and types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define white 'white)
(define black 'black)

(define pawn 'pawn)
(define rook 'rook)
(define knight 'knight)
(define bishop 'bishop)
(define queen 'queen)
(define king 'king)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All pieces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define white-pawn (piece white pawn))
(define white-rook (piece white rook))
(define white-knight (piece white knight))
(define white-bishop (piece white bishop))
(define white-queen (piece white queen))
(define white-king (piece white king))

(define black-pawn (piece black pawn))
(define black-rook (piece black rook))
(define black-knight (piece black knight))
(define black-bishop (piece black bishop))
(define black-queen (piece black queen))
(define black-king (piece black king))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vectors to be used for board setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; White major pieces
(define init-rank1 (list white-rook white-knight white-bishop
                         white-queen white-king white-bishop
                         white-knight white-rook))
;;; White pawns
(define init-rank2 (make-list 8 white-pawn))

;;; Black pawns
(define init-rank7 (make-list 8 black-pawn))

;;; Black major pieces 
(define init-rank8 (list black-rook black-knight black-bishop
                         black-queen black-king black-bishop
                         black-knight black-rook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          Unicode (For CLI board)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define black-pawn-unicode "♙")
(define black-rook-unicode "♖")
(define black-knight-unicode "♘")
(define black-bishop-unicode "♗")
(define black-queen-unicode "♕")
(define black-king-unicode "♔")

(define white-pawn-unicode "♟")
(define white-rook-unicode "♜")
(define white-knight-unicode "♞")
(define white-bishop-unicode "♝")
(define white-queen-unicode "♛")
(define white-king-unicode "♚")

(define (piece->unicode p)
  (cond
    [(equal? p white-king) white-king-unicode]
    [(equal? p white-queen) white-queen-unicode]
    [(equal? p white-rook) white-rook-unicode]
    [(equal? p white-bishop) white-bishop-unicode]
    [(equal? p white-knight) white-knight-unicode]
    [(equal? p white-pawn) white-pawn-unicode]
    
    [(equal? p black-king) black-king-unicode]
    [(equal? p black-queen) black-queen-unicode]
    [(equal? p black-rook) black-rook-unicode]
    [(equal? p black-bishop) black-bishop-unicode]
    [(equal? p black-knight) black-knight-unicode]
    [(equal? p black-pawn) black-pawn-unicode]
    [else " "]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     Recognizers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pawn? p)
  (equal? (piece-type p) pawn))

(define (rook? p)
  (equal? (piece-type p) rook))

(define (knight? p)
  (equal? (piece-type p) knight))

(define (bishop? p)
  (equal? (piece-type p) bishop))

(define (queen? p)
  (equal? (piece-type p) queen))

(define (king? p)
  (equal? (piece-type p) king))

(define (white? p)
  (equal? (piece-color p) white))

(define (black? p)
  (equal? (piece-color p) black))
