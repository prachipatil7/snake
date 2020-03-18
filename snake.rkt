;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./snake_lib.rkt")

; a game is...
; - (make-game snake (listof posn) (listof posn) number)
; (define-struct game (snake food obstacles ticks))

; a direction is one of...
; - 'up
; - 'down
; - 'left
; - 'right
; If this type looks new to you, its just a symbol.
; That is ‘up is a symbol and “up” is a string.
; Symbols are like strings without spaces. 

; a snake is...
; - (make-snake direction (listof posn))
; (define-struct snake (heading segments))

; segments is either
; - (cons posn empty)
; - (cons posn segments)
; That is, segments is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-food : game posn -> game
; Given a game and posn, returns a new game where food has been added
;   at that posn. 
(define (add-food g p)
  (make-game (game-snake g)
             (cons p (game-food g))
             (game-obstacles g)
             (game-ticks g)))

(check-expect
 (add-food (make-game (make-snake 'up (list (make-posn 1 2)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      5)
           (make-posn 6 7))
 (make-game (make-snake 'up (list (make-posn 1 2)))
            (list (make-posn 6 7) (make-posn 3 4))
            (list (make-posn 10 10) 
                  (make-posn 20 20))
            5))

; change-direction : game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 
(define (change-direction g d)
  (make-game (make-snake d (snake-segments (game-snake g)))
             (game-food g)
             (game-obstacles g)
             (game-ticks g)))

(check-expect
 (change-direction
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'left)
 (make-game (make-snake 'left (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5))

; game-score : game -> number
; Given a game, returns a score (as a number)
(define (game-score g)
  (- (* (length (snake-segments (game-snake g))) 100)
     (game-ticks g)))

(check-expect
 (game-score
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5))
 95)

;self-death? game -> boolean
;returns true if the given game is the snake running into itself
(define (self-death? g)
  (and (> (length (snake-segments (game-snake g))) 2)
       (common-posn? 
        (direction-based (snake-heading (game-snake g))
                         (snake-segments (game-snake g)))
        (snake-segments (game-snake g))))) 
       
;wall-death? game -> boolean
;returns true if the given game includes running into a wall
(define (wall-death? g)
  (or (> (posn-x (first (snake-segments (game-snake g)))) 50)
      (< (posn-x (first (snake-segments (game-snake g)))) 0)
      (> (posn-y (first (snake-segments (game-snake g)))) 50)
      (< (posn-y (first (snake-segments (game-snake g)))) 0)))

;common-posn? posn, (listof posn) -> boolean
;returns true if the posn is in one of the lists
(define (common-posn? p lst)
  (ormap (lambda (p2)
           (equal? p p2))
         lst))

;obstacle-death? game -> boolean
;returns true if the game includes running into an obstacle
(define (obstacle-death? g)
  (common-posn? (first (snake-segments (game-snake g)))
                (game-obstacles g)))
  
; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise. 
(define (game-over? g)
  (or (self-death? g)
      (wall-death? g)
      (obstacle-death? g)))

(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn -1 1))) empty empty 5))
 true)

;remove-last list -> list
;returns a list without the last term
(define (remove-last lst)
  (reverse (rest (reverse lst))))

(check-expect (remove-last (list 1 2 3 4))
              (list 1 2 3))

(check-expect (remove-last (list 1 2 3 4 5 5))
              (list 1 2 3 4 5))                         

;direction-based direction, (listof posn) -> posn
;returns the next posn based on the direction the snake is heading
(define (direction-based head lst)
  (cond   [(equal? head 'up)
           (make-posn (posn-x (first lst))
                      (+ 1 (posn-y (first lst))))]
          [(equal? head 'down)
           (make-posn (posn-x (first lst))
                      (- (posn-y (first lst)) 1))]
          [(equal? head 'right)
           (make-posn (+ 1 (posn-x (first lst)))
                      (posn-y (first lst)))]
          [(equal? head 'left)
           (make-posn (- (posn-x (first lst)) 1)
                      (posn-y (first lst)))]))

; advance-game : game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not. 
(define (advance-game g)
   (local [(define dir (snake-heading (game-snake g)))
           (define segs (snake-segments (game-snake g)))]
     (if (common-posn? (direction-based dir segs)
                       (game-food g))
         (make-game (make-snake dir
                                (cons (direction-based dir segs)
                                      segs))
                    (remove (direction-based dir segs) (game-food g))
                    (game-obstacles g)
                    (+ (game-ticks g) 1))
         (make-game (make-snake dir
                                (cons (direction-based dir segs)
                                      (remove-last segs)))
                    (game-food g)
                    (game-obstacles g)
                    (+ (game-ticks g) 1))))) 
                                                      
(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             empty
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)))
            empty
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
(check-expect
 (advance-game
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             (list (make-posn 2 1) (make-posn 8 9))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)
                                    (make-posn 3 3)))
            (list (make-posn 8 9))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))

; a starting game to experiment with
(define game-start
  (make-game (make-snake 'up (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game advance-game add-food change-direction game-score game-over?)) 

;to start a game
;(play game-start)

;to make sure all procedures are defined with no typos
(check-expect (procedure? add-food) #true)
(check-expect (procedure? change-direction) #true)
(check-expect (procedure? game-score) #true)
(check-expect (procedure? game-over?) #true)
(check-expect (procedure? advance-game) #true)