#lang racket/gui

(define metropolitan-line '("-Choose a Location-" "Aldgate" "Liverpool Street" "Moorgate" "Barbican" "Farringdon" "Kings Cross St. Pancras"
                                      "Euston Square" "Great Portland Street" "Baker Street" "Finchley Road" "Wembley Park"
                                      "Preston Road" "Northwick Park" "Harrow-on-the-Hill" "North Harrow" "West Harrow" "Eastcote"
                                      "Pinner" "Northwood Hills" "Ruislip Manor" "Ruislip" "Northwood" "Ickenham" "Moor Park"
                                      "Hillingdon" "Croxley" "Rickmansworth" "Uxbridge" "Watford" "Chorleywood" "Chalfont and Latimer"
                                      "Chesham" "Amersham"))

(define metropolitan-graph '(("Chesam" "Chalfont & Latimer") ("Chalfont & Latimer" "Chesam")
                             ("Amersham" "Chalfont & Latimer") ("Chalfont & Latimer" "Amersham")
                             ("Chalfont & Latimer" "Chorleywood") ("Chorleywood" "Chalfont & Latimer")
                             ("Chorleywood" "Rickmansworth") ("Rickmansworth" "Chorleywood")
                             ("Rickmansworth" "Moor Park") ("Moor Park" "Rickmansworth")
                             ("Watford" "Croxley") ("Croxley" "Watford")
                             ("Croxley" "Moor Park") ("Moor Park" "Croxley")
                             ("Moor Park" "Northwood") ("Northwood" "Moor Park")
                             ("Northwood" "Northwood Hills") ("Northwood Hills" "Northwood")
                             ("Northwood Hills" "Pinner") ("Pinner" "Northwood Hills")
                             ("Pinner" "North Harrow") ("North Harrow" "Pinner")
                             ("North Harrow" "Harrow-on-the-Hill") ("Harrow-on-the-Hill" "North Harrow")
                             ("Uxbridge" "Hillingdon") ("Hillingdon" "Uxbridge")
                             ("Hillingdon" "Ickenham") ("Ickenham" "Hillingdon")
                             ("Ickenham" "Ruislip") ("Ruislip" "Ickenham")
                             ("Ruislip" "Ruislip Manor") ("Ruislip Manor" "Ruislip")
                             ("Ruislip Manor" "Eastcote") ("Eastcote" "Ruislip Manor")
                             ("Eastcote" "Rayners Lane") ("Rayners Lane" "Eastcote")
                             ("Rayners Lane" "West Harrow") ("West Harrow" "Rayners Lane")
                             ("West Harrow" "Harrow-on-the-Hill") ("Harrow-on-the-Hill" "West Harrow")
                             ("Harrow-on-the-Hill" "Northwick Park") ("Northwick Park" "Harrow-on-the-Hill")
                             ("Northwick Park" "Preston Road") ("Preston Road" "Northwick Park")
                             ("Preston Road" "Wembley Park") ("Wembley Park" "Preston Road")
                             ("Wembley Park" "Finchley Road") ("Finchley Road" "Wembley Park")
                             ("Finchley Road" "Baker Street") ("Baker Street" "Finchley Road")
                             ("Baker Street" "Great Portland Street") ("Great Portland Street" "Baker Street")
                             ("Great Portland Street" "Euston Square") ("Euston Square" "Great Portland Street")
                             ("Euston Square" "King's Cross St. Pancras") ("King's Cross St. Pancras" "Euston Square")
                             ("King's Cross St. Pancras" "Farringdon") ("Farringdon" "King's Cross St. Pancras")
                             ("Farringdon" "Barbican") ("Barbican" "Farringdon")
                             ("Barbican" "Moorgate") ("Moorgate" "Barbican")
                             ("Moorgate" "Liverpool Street") ("Liverpool Street" "Moorgate")
                             ("Liverpool Street" "Aldgate") ("Aldgate" "Liverpool Street")))


(define edge (λ (x graph)
    (map (λ (y) (first (rest y))) (filter(λ (y)
             (equal? (first y) x)) graph))))

(define path (λ (x y a-graph vertex-set)
    (cond
      [(equal? x y) #t]
      [(not (set-member? vertex-set x)) #f]
      [(not (set-member? vertex-set y)) #f]
      [#t (ormap (λ (z)(path z y a-graph (set-remove vertex-set x )))
                                         (edge x a-graph ))]
      )))

(define reachable(λ (x y a-graph)
     (path x y a-graph (list->set (flatten a-graph)))))

;;;;;;;;;;;;;;;;;;; GUI ;;;;;;;;;;;;;;;;;
(define window
  (new frame%
       [label "Route Planner"]
       [width 400]
       [height 300]
       [style '(fullscreen-button)]
       [alignment '(center top)]
       ))


(define welcomemsg (new message%
     [parent window]
     [label "Welcome to your Route Planner"]
     [vert-margin 10]))

(define panel (new horizontal-pane%
     [parent window]
     [vert-margin 5]
     [horiz-margin 10]
     [alignment '(center top)]
     [stretchable-width #t]
     [stretchable-height #f]))

(define from (new choice%
    [parent panel]
    [label "From: "]
    [horiz-margin 10]
    [choices metropolitan-line]))

(define to (new choice%
    [parent panel]
    [label "To: "]
    [horiz-margin 10]
    [choices metropolitan-line]))


(define planroutebutton (new button%
    [parent window]
    [vert-margin 10]
    [label "Plan my route"]
    [callback (λ (o e)
       (displayln (send from get-string-selection))
       (displayln (send to get-string-selection))
       (display(reachable (send from get-string-selection) (send to get-string-selection) metropolitan-graph)))]))

  
(define route-output (new editor-canvas%
    [parent window]
    [style '(no-vscroll no-hscroll)]
    [label "Your Route"]
    [min-width 300]
    [min-height 100]
    [horiz-margin 20]
    [stretchable-width #t]
    [stretchable-height #f]))

(send window show #t)

