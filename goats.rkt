#lang racket

; rob fitzgerald
; s# 103400432
; csci5800 spring 2016

(require "graph-search.rkt")

;;; csci 5800 assignment 2: Black and White Mountain Goat Dance Party

;;;  problem statement
 
; Three black mountain goats one direction on a narrow mountain ledge and encounter three white mountain goats 
; going in the opposite direction. They stop with the space of a goat between the two lead animals. Mountain goats 
; cannot (for the purposes of the puzzle) move backwards, but they are good jumpers. A mountain goat can either 
; move forward into an empty space, jump over one mountain goat to an empty space, or jump over two mountain goats 
; to an empty space. How can they all continue on their way?

; We can abstract this to the following: B B B _ W W W

; Where each B represents a black mountain goat, each W represents a white mountain goat, and _ represents a blank 
; space. Each B may move right into an adjacent _, jump over an intervening B or W into an _, or jump over two 
; intervening B or W into an _. Each W may similarly move left the same way. The goal is to get all of the Bs to 
; the right of all of the Ws.

; Use the graph search program provided in class to build a state space solution to the problem. Is there a 
; difference in the solution for a breadth-first versus depth-first traversal of the graph? What is the difference 
; in the number of node generated and explored?

; Extend your representation to handle any number of black and white goats – they don’t have to be the same number.
; Is the problem solvable for all such combinations?

;;; containers

(define _ '_)
(define B 'B)
(define W 'W)
(define (new-state p0 p1 p2 p3 p4 p5 p6) 
  (vector p0 p1 p2 p3 p4 p5 p6))
(define (copy-state state)
  (vector-copy state))
;define state as vector.
  
;;; predicates
  
(define (_? a)
  (eq? a '_))
(define (B? a)
  (eq? a 'B))
(define (W? a)
  (eq? a 'W))
(define (in-range? num)
  (and (>= num 0)
       (<= num 7)))

;;; queries & query helpers.

;;; (at state pos) => any/c
(define (at state pos)
  (vector-ref state pos))

;;; (at-space state) => number?
(define (at-space state)
  (vector-member _ state))

(define (left-of num)
  (- num 1))
(define (two-left-of num)
  (- num 2))
(define (three-left-of num)
  (- num 3))
(define (right-of num)
  (+ num 1))
(define (two-right-of num)
  (+ num 2))
(define (three-right-of num)
  (+ num 3))

;;; mutators

(define (swap vec i j)
  (define temp (vector-ref vec i))
  (vector-set! vec i (vector-ref vec j))
  (vector-set! vec j temp))
  
;;; rules

(define-ruleset goats-rule)

; B _
(define-rule goats-rule (B-step-1 state)
  (in-range? (left-of (at-space state)))
=>
  (swap (copy-state state) (at-space state) (left-of (at-space state))))

; _ W
(define-rule goats-rule (W-step-1 state)
  (in-range (right-of (at-space state)))
=>
  (swap (copy-state state) (at-space state) (right-of (at-space state))))

; B * _
(define-rule goats-rule (B-jump-1 state)
  (and (in-range? (two-left-of (at-space state)))
       (B? (at state (two-left-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (two-left-of (at-space state))))

; _ * W
(define-rule goats-rule (W-jump-1 state)
  (and (in-range? (two-right-of (at-space state)))
       (W? (at state (two-right-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (two-right-of (at-space state))))

; B * * _
(define-rule goats-rule (B-jump-2 state)
  (and (in-range? (three-left-of (at-space state)))
       (B? (at state (three-left-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (three-left-of (at-space state))))

; _ * * W
(define-rule goats-rule (W-jump-2 state)
  (and (in-range? (three-right-of (at-space state)))
       (W? (at state (three-right-of (at-space state)))))
=>
  (swap (copy-state state) (at-space state) (three-right-of (at-space state))))

;;; search

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