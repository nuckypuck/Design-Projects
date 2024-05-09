#lang racket/gui
(require plot)

(define xs '(0 1 2 3 4 5))
(define ys '(5 1 4 9 16 25))

(plot 
  (list 
    (points (map vector xs ys) #:color 'red)
    (lines (map vector xs ys) #:color 'blue))
  #:x-label "Custom X-Axis Label"
  #:y-label "")
