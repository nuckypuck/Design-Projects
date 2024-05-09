#lang racket/gui

(define (draw-A dc x y)
  (send dc draw-text "A" x y))

(define (draw-B dc x y)
  (send dc draw-text "B" x y))

(define (on-paint canvas dc)
  (draw-A dc 50 50)
  (draw-B dc 100 50)
  (send dc draw-line 60 60 100 60))

(define frame (new frame%
                 [label "A and B with Line"]
                 [width 500]
                 [height 500]))

(define canvas (new canvas%
                   [parent frame]
                   [paint-callback on-paint]))

(send frame show #t)
