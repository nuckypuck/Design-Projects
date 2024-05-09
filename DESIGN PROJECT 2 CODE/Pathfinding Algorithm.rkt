#lang racket/gui
(require (planet jaymccarthy/dijkstra:1:1))
(require plot)

(define-struct edge (end weight))
  (define-struct graph (adj))
  
  (define (create-graph)
   (make-graph (make-hasheq)))
  (define (graph-add! g start end [weight 0])
   (hash-update! (graph-adj g)
                 start
                 (lambda (old)
                   (list* (make-edge end weight) old))
                 empty))
  (define (graph-shortest-path g src [stop? (lambda (n) #f)])
   (shortest-path (lambda (n) (hash-ref (graph-adj g) n empty))
                  edge-weight
                  edge-end
                  src
                  stop?))
  (define g (create-graph))

(graph-add! g "Station 1" "Station 2" 4)
(graph-add! g "Station 2" "Station 3" 5)
(graph-add! g "Station 3" "Station 4" 6)
(graph-add! g "Station 2" "Station 1" 4)
(graph-add! g "Station 3" "Station 2" 5)
(graph-add! g "Station 4" "Station 3" 6)

(define (print-shortest-path g src dest)
  (define-values (dist prev) (graph-shortest-path g src))
  (define path (shortest-path-to prev dest))
  (define total-time (hash-ref dist dest))
  (display "Your Route is ")
  (display path)
  (display ", then your final stop is ")
  (display dest)
  (display ". The total trip time is ")
  (display total-time)
  (display " minutes")
  (newline))

(define routeplanner (λ (x y) (print-shortest-path g x y)))

;;;;;;;;;;;;;;GUI;;;;;;;;;;;;;;;;;;;;;;;;;
(define frame (new frame%
                  [label "Route Finder"]
                  [width 300]
                  [height 200]))

(define panel (new vertical-panel%
                  [parent frame]))

(define start-locations '( "Station 1" "Station 2" "Station 3" "Station 4"))
(define end-locations '( "Station 1" "Station 2" "Station 3" "Station 4"))

(define start-dropdown (new choice%
                        [label "Start Location:"]
                        [choices start-locations]
                        [parent panel]))

(define end-dropdown (new choice%
                      [label "End Location:"]
                      [choices end-locations]
                      [parent panel]))

(define find-route-button (new button%
                           [label "Find My Route"]
                           [parent panel]
                           [callback (lambda (button event)
                                       (routeplanner (send start-dropdown get-string-selection)
                                                     (send end-dropdown get-string-selection)))]))
  

(send frame show #t)








