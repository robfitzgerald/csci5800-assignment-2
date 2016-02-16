#lang racket

; goats.rkt
; graph search program for the Mountain Goat Dance Party Problem

; rob fitzgerald
; s# 103400432
; csci5800 spring 2016

(require "graph-search.rkt")

; see a2-writeup.txt for written responses to questions in the assignment.


;;;;;;;;;;;;;;;;;;
;;; containers ;;;
;;;;;;;;;;;;;;;;;;

;;; note: over-writing built-in keyword _ used for "match any" syntax object.
(define _ '_)
(define B 'B)
(define W 'W)

;;; (new-state . pieces) -> vector?
;;;   pieces: list?
;;; state space arguments for beginning the graph search
(define (new-state . pieces)
  (list->vector pieces))

;;; (copy-state state) -> vector?
;;;   state: vector?
;;; creates a copy in memory of the provided vector
(define (copy-state state)
  (vector-copy state))

;;;;;;;;;;;;;;;;;;  
;;; predicates ;;;
;;;;;;;;;;;;;;;;;;

;;; (_? a) -> boolean?
;;;   a: any?
;;; does the value match the symbol for the empty space
(define (_? a)
  (eq? a '_))

;;; (B? a) -> boolean?
;;;   a: any?
;;; does the value match the symbol for the black goat
(define (B? a)
  (eq? a 'B))

;;; (W? a) -> boolean?
;;;   any?
;;; does the value match the symbol for the white goat
(define (W? a)
  (eq? a 'W))

;;; (in-range? num limit) -> boolean?
;;;   num: number?
;;;   limit: number?
;;; bounds check for the vector containing the search state
(define (in-range? num limit)
  (and (number? num)
       (>= num 0)
       (< num limit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; queries & query helpers. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (state-length state) => number?
;;;   state: vector?
;;; alias for vector-length: count of elements in the search space
(define (state-length state)
  (vector-length state))

;;; (at state pos) => any/c
;;;   state: vector?
;;;   pos: number?
;;; alias for vector-ref,
;;; returns the symbol that is stored at the position pos in the vector state.
(define (at state pos)
  (vector-ref state pos))

;;; (at-space state) => number?
;;;   state: vector?
;;; returns the position of the first (of what should only be one) 
;;; underscore character in the state space
(define (at-space state)
  (vector-member _ state))

;;; these six functions determine index values for adjacent and near-adjacent vector indices

;;; (left-of num) -> number?
;;;   num: number?
(define (left-of num)
  (- num 1))

;;; (two-left-of num) -> number?
;;;   num: number?
(define (two-left-of num)
  (- num 2))

;;; (three-left-of num) -> number?
;;;   num: number?
(define (three-left-of num)
  (- num 3))

;;; (right-of num) -> number?
;;;   num: number?
(define (right-of num)
  (+ num 1))

;;; (two-right-of num) -> number?
;;;   num: number?
(define (two-right-of num)
  (+ num 2))

;;; (three-right-of num) -> number?
;;;   num: number?
(define (three-right-of num)
  (+ num 3))

;;;;;;;;;;;;;;;;
;;; mutators ;;;
;;;;;;;;;;;;;;;;

;;; (swap vec i j) -> vector?
;;;   vec: vector?
;;;   i: number?
;;;   j: number?
;;; traditional vector swap function.
(define (swap vec i j)
  (define temp (vector-ref vec i))
  (vector-set! vec i (vector-ref vec j))
  (vector-set! vec j temp)
  vec)
  
;;;;;;;;;;;;;
;;; rules ;;;
;;;;;;;;;;;;;

(define-ruleset goats-rule)

; B _
(define-rule goats-rule (B-step-1 state)
  (and (in-range? (left-of (at-space state)) (state-length state))
       (in-range? (at-space state) (state-length state))
       (B? (at state (left-of (at-space state)))))
       
=>
  (swap (copy-state state) (at-space state) (left-of (at-space state))))

; _ W
(define-rule goats-rule (W-step-1 state)
  (and (in-range? (right-of (at-space state)) (state-length state))
       (in-range? (at-space state) (state-length state))
       (W? (at state (right-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (right-of (at-space state))))

; B * _
(define-rule goats-rule (B-jump-1 state)
  (and (in-range? (two-left-of (at-space state)) (state-length state))
       (in-range? (at-space state) (state-length state))
       (B? (at state (two-left-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (two-left-of (at-space state))))

; _ * W
(define-rule goats-rule (W-jump-1 state)
  (and (in-range? (two-right-of (at-space state)) (state-length state))
       (in-range? (at-space state) (state-length state))
       (W? (at state (two-right-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (two-right-of (at-space state))))

; B * * _
(define-rule goats-rule (B-jump-2 state)
  (and (in-range? (three-left-of (at-space state)) (state-length state))
       (in-range? (at-space state) (state-length state))
       (B? (at state (three-left-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (three-left-of (at-space state))))

; _ * * W
(define-rule goats-rule (W-jump-2 state)
  (and (in-range? (three-right-of (at-space state)) (state-length state))
       (in-range? (at-space state) (state-length state))
       (W? (at state (three-right-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (three-right-of (at-space state))))

;;; graph-search interface / runner

(define (goats-search initial-state goal-state merge-method)
  (printf "(goats-search ~a ~a '~a)~n" initial-state goal-state merge-method)
  (define (goats-value state cost)
    cost)
  (define (goal? state)
    (equal? state goal-state))
  (define solution
    (graph-search goats-rule initial-state goal?
                  #:merge-method merge-method))
  (cond (solution
         (printf "Solution (~a states): ~n" (length solution))
         (for ((state (in-list solution)))
           (printf "~s~n" state)))
         (else
          (printf "No solution found.~n")))
        (printf "---~n~n"))



(goats-search (new-state B B B _ W W W) (new-state W W W _ B B B) 'prepend)
(goats-search (new-state B B B _ W W W) (new-state W W W _ B B B) 'append)

; solves for any size state. does not need to be symmetrical, but 
; all goats of each type must be on either side of the space (as in the problem description).
;(goats-search (new-state B B B B B B B B B B _ W W W W) (new-state W W W W _ B B B B B B B B B B) 'append)