#lang racket
(require racket/gui)

(define no-of-particles 100)
(define no-of-resources 3)
(define (no-of-mines-func resource-number) 3)
(define no-of-mines-vector
  (build-vector
   no-of-resources
   (lambda (res-num)
     (no-of-mines-func (+ res-num 1)))))

(define welcomeframe (new frame%
                          [label "Welcome to the Game"]
                          [width 1300]
                          [height 710]))
(define secondFrame (new frame%
                          [label "Enter the number of mines of each resource."]
                          [width 1300]
                          [height 710]))


(define my-text-field%
  (class text-field%
    (super-new)
    ))

(define msg1 (new message%
                 [parent welcomeframe]
                 [label "The rules of the game is as follows:"]))

(define msg2 (new message%
                 [parent welcomeframe]
                 [label "1. You have to input the current value of each resource and the number of mines of each resource."]))

(define msg3 (new message%
                 [parent welcomeframe]
                 [label "2. Then for each resource you need to enter the number of mines and the level of each mine."]))

(define msg4 (new message%
                 [parent welcomeframe]
                 [label "3. The output will be the upgrades you have to perform to get the optimised result in the end."]))

(define msg5 (new message%
                 [parent welcomeframe]
                 [label "4. If the level of mine is n then it produces n amount of that resource, and to upgrade that resources we need n resources of each type."]))

(new button% [parent welcomeframe] 
             [label "Enter for giving the input."]
             [callback (lambda (button event)
                         (begin (send welcomeframe show #f) (send secondFrame show #t)))])
(new button% [parent secondFrame] 
             [label "Enter for giving the status of each resource."]
             [callback (lambda (button event)
                          (f) )])
(define field-gm (new my-text-field% [label "Number of gold mines"] [parent secondFrame]
                     [callback (lambda (x y) (vector-set! no-of-mines-vector 0 (car (read (open-input-string (string-append "(" (send field-gm get-value) ")"))))))]))
(define field-im (new my-text-field% [label "Number of iron mines"] [parent secondFrame]
                     [callback (lambda (x y) (vector-set! no-of-mines-vector 1 (car (read (open-input-string (string-append "(" (send field-im get-value) ")"))))))]))
(define field-wm (new my-text-field% [label "Number of wood mines"] [parent secondFrame]
                     [callback (lambda (x y) (vector-set! no-of-mines-vector 2 (car (read (open-input-string (string-append "(" (send field-wm get-value) ")"))))))]))

(send welcomeframe show #t)

;;;;;;;;;;;;;;;;;;;
;INPUTS
;;;;;;;;;;;;;;;;;;;

(define (initial-value-of-resources resource-number) 10)
(define (initial-level-of-mines resource-number mine-number) 0)
(define (total-no-of-upgrades-func resource-number mine-number) 5)
(define (acc-rate-func resource-number mine-number upgrade-number) (+ upgrade-number 1))
(define (upgrade-time-func resource-number mine-number upgrade-number) 0.5)
(define (cost-func resource-number mine-number upgrade-number)
  (if (= resource-number 1)
      (vector upgrade-number (* 2 upgrade-number) upgrade-number)
      (vector (* 3 upgrade-number) upgrade-number upgrade-number)))

(define (f)

(define thirdFrame (new frame%
                          [label "Enter the current value of each resource."]
                          [width 1300]
                          [height 710]))
(define HIGH-VAL 100000)
(define lowerbound 0)
(define upperbound 10)
;number of mines
;initial state of each resource
;level of each mine
;Number of resources

(define (helper0 n1 n2)
  (define field-gml (new my-text-field% [label (string-append "Level of gold mine number" (number->string n1))] [parent thirdFrame]
                         [callback (lambda (x y) (vector-set! (cdr (vector-ref initial-state-vector 0)) (- n1 1) (car (read (open-input-string (string-append "(" (send field-gml get-value) ")"))))))]))
  (if (= n1 n2) '() (helper0 (+ n1 1) n2) ))
(define (helper1 n1 n2)
  (define field-gml (new my-text-field% [label (string-append "Level of iron mine number" (number->string n1))] [parent thirdFrame]
                         [callback (lambda (x y) (vector-set! (cdr (vector-ref initial-state-vector 1)) (- n1 1) (car (read (open-input-string (string-append "(" (send field-gml get-value) ")"))))))]))
  (if (= n1 n2) '() (helper1 (+ n1 1) n2) ))
(define (helper2 n1 n2)
  (define field-gml (new my-text-field% [label (string-append "Level of wood mine number" (number->string n1))] [parent thirdFrame]
                         [callback (lambda (x y) (vector-set! (cdr (vector-ref initial-state-vector 2)) (- n1 1) (car (read (open-input-string (string-append "(" (send field-gml get-value) ")"))))))]))
  (if (= n1 n2) '() (helper2 (+ n1 1) n2) ))
 
  (define field-gs (new my-text-field% [label "Gold status"] [parent thirdFrame]
                     [callback (lambda (x y) (vector-set! initial-state-vector 0 (cons (car (read (open-input-string (string-append "(" (send field-gs get-value) ")"))))
                                                                                       (cdr (vector-ref initial-state-vector 0)))))]))
 (helper0 1 (vector-ref no-of-mines-vector 0))
  (define field-is (new my-text-field% [label "Iron status"] [parent thirdFrame]
                     [callback (lambda (x y) (vector-set! initial-state-vector 1 (cons (car (read (open-input-string (string-append "(" (send field-is get-value) ")"))))
                                                                                       (cdr (vector-ref initial-state-vector 1)) )))]))
 (helper1 1 (vector-ref no-of-mines-vector 1))

  (define field-ws (new my-text-field% [label "Wood status"] [parent thirdFrame]
                     [callback (lambda (x y) (vector-set! initial-state-vector 2 (cons (car (read (open-input-string (string-append "(" (send field-ws get-value) ")"))))
                                                                                       (cdr (vector-ref initial-state-vector 2)) )))]))

 (helper2 1 (vector-ref no-of-mines-vector 2))
(new button% [parent thirdFrame] 
             [label "Enter for getting the output"]
             [callback (lambda (button event) (begin (initiate-particles)  (display (PSO 10))))])

;;;;;;;;;;;;;;;;;
;VECTORS
;;;;;;;;;;;;;;;;;;
(define particle-vector (make-vector no-of-particles #f))
;each particle has n vectors. n is the no-of-resources
;each element contains more vectors, each representing 1 mine of the resource
;each elelment is a vector that stores the upgrade times.
;this is initial level of each resource
;this s the number of mines


(define initial-state-vector 
  (build-vector
   no-of-resources
   (lambda (res-num)
     (cons (initial-value-of-resources (+ res-num 1))
           (build-vector
            ;(no-of-mines (+ res-num 1))
            (vector-ref no-of-mines-vector res-num)
            ;
            (lambda (mine-num)
              (initial-level-of-mines (+ res-num 1) (+ mine-num 1))))))))

(define total-no-of-upgrades-vector 
  (build-vector
   no-of-resources
   (lambda (res-num)
     (build-vector
      ;(no-of-mines (+ res-num 1))
      (vector-ref no-of-mines-vector res-num)
      ;
      (lambda (mine-num)
        (+ mine-num 5))))))

(define upgrade-time-vector 
  (build-vector
   no-of-resources
   (lambda (res-num)
     (build-vector
      ;(no-of-mines (+ res-num 1))
      (vector-ref no-of-mines-vector res-num)
      ;
      (lambda (mine-num)
        (build-vector
         ;(total-no-of-upgrades (+ res-num 1) (+ mine-num 1))
         (vector-ref
          (vector-ref
           total-no-of-upgrades-vector
           res-num)
          mine-num)
         ;
         (lambda (up-num)
           (upgrade-time-func (+ res-num 1) (+ mine-num) (+ up-num 1)))))))))

(define upgrade-cost-vector
  (build-vector
   no-of-resources
   (lambda (res-num)
     (build-vector
      ;(no-of-mines (+ res-num 1))
      (vector-ref no-of-mines-vector res-num)
      ;
      (lambda (mine-num)
        (build-vector
         ;(total-no-of-upgrades (+ res-num 1) (+ mine-num 1))
         (vector-ref
          (vector-ref
           total-no-of-upgrades-vector
           res-num)
          mine-num)
         ;
         (lambda (up-num)
           (cost-func (+ res-num 1) (+ mine-num 1) (+ up-num 1)))))))))

(define acc-rate-vector
  (build-vector
   no-of-resources
   (lambda (res-num)
     (build-vector
      ;(no-of-mines (+ res-num 1))
      (vector-ref no-of-mines-vector res-num)
      ;
      (lambda (mine-num)
        (build-vector
         ;(total-no-of-upgrades (+ res-num 1) (+ mine-num 1))
         (vector-ref
          (vector-ref
           total-no-of-upgrades-vector
           res-num)
          mine-num)
         ;
         (lambda (up-num)
           (acc-rate-func (+ res-num 1) (+ mine-num 1) (+ up-num 1)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;USEFUL FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;These don't depend on initial state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (particle n)
  (vector-ref particle-vector (- n 1)))

(define (initial-level resource-number mine-number)
  (vector-ref
   (cdr (vector-ref initial-state-vector (- resource-number 1)))
   (- mine-number 1)))

(define (initial-amount resource-number)
  (car (vector-ref initial-state-vector (- resource-number 1))))

(define (no-of-mines resource-number)
  (vector-ref no-of-mines-vector (- resource-number 1)))

(define (total-no-of-upgrades resource-number mine-number)
  (if (> resource-number no-of-resources) #f
      (if (> mine-number (no-of-mines resource-number)) #f
          (vector-ref
           (vector-ref
            total-no-of-upgrades-vector
            (- resource-number 1))
           (- mine-number 1)))))

(define (no-of-upgrades
         resource-number
         mine-number)
  (if (> resource-number no-of-resources) #f
      (if (> mine-number (no-of-mines resource-number)) #f
          (- (total-no-of-upgrades resource-number mine-number)
             (initial-level resource-number mine-number)))))

(define (upgrade-time
         resource-number
         mine-number
         upgrade-number)
  (if (> resource-number no-of-resources) #f
      (if (> mine-number (no-of-mines resource-number)) #f
          (if (>= 0 upgrade-number) 0
              (if (> upgrade-number (total-no-of-upgrades resource-number mine-number)) #f
                  (vector-ref
                   (vector-ref
                    (vector-ref
                     upgrade-time-vector
                     (- resource-number 1))
                    (- mine-number 1))
                   (- upgrade-number 1)))))))

(define (upgrade-cost
         resource-number
         mine-number
         upgrade-number)
  (if (> resource-number no-of-resources) #f
      (if (> mine-number (no-of-mines resource-number)) #f
          (if (>= 0 upgrade-number) (make-vector no-of-resources 0)
              (if (> upgrade-number (total-no-of-upgrades resource-number mine-number)) #f
                  (vector-ref
                   (vector-ref
                    (vector-ref
                     upgrade-cost-vector
                     (- resource-number 1))
                    (- mine-number 1))
                   (- upgrade-number 1)))))))

;;;;;;;;;;;;;;;;;;;;;;
;MORE USEFUL FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;
;These depend on initial state
;;;;;;;;;;;;;;;;;;;;;;

(define (resource-time-get
         resource-number;starts from 1
         mine-number;starts from 1
         upgrade-number;starts from 1
         particle)
  (if (> resource-number no-of-resources) #f
      (if (<= upgrade-number (initial-level resource-number mine-number)) (- 0 (upgrade-time resource-number mine-number upgrade-number))
          (if (> upgrade-number (total-no-of-upgrades resource-number mine-number)) HIGH-VAL
              (vector-ref
               (vector-ref
                (vector-ref
                 particle
                 (- resource-number 1))
                (- mine-number 1))
               (- (- upgrade-number 1)
                  (initial-level
                   resource-number
                   mine-number)))))))

(define (resource-time-set!
         resource-number;starts from 1
         mine-number;starts from 1
         upgrade-number;starts from 1
         particle
         new-value
         )
  (if (> resource-number no-of-resources) #f
      (if (> mine-number (no-of-mines resource-number)) #f
          (if (= upgrade-number (initial-level resource-number mine-number)) #f
              (if (> upgrade-number (total-no-of-upgrades resource-number mine-number)) #f
                  (vector-set!
                   (vector-ref
                    (vector-ref
                     particle
                     (- resource-number 1))
                    (- mine-number 1))
                   (- (- upgrade-number 1)
                      (initial-level
                       resource-number
                       mine-number))
                   new-value))))))

(define (acc-rate-of-mine resource-number mine-number upgrade-number)
  (if (= upgrade-number 0) 0.1
      (vector-ref
       (vector-ref
        (vector-ref
         acc-rate-vector
         (- resource-number 1))
        (- mine-number 1))
       (- upgrade-number 1))))

(define (acc-rate resource-number time particle)
  (define curr-mine 1)
  (define sum 0)
  (define (find-latest-upgrade-number)
    (define curr-up 0)
    (define curr-min (- 0 HIGH-VAL))
    (define (helper)
      (if (> curr-mine (no-of-mines resource-number)) #f
          (let ([t (resource-time-get resource-number curr-mine (+ curr-up 1) particle)])
            (if (> t time)
                curr-up
                (begin (set! curr-up (+ curr-up 1))
                       (set! curr-min t)
                       (helper))))))
    (helper))
  (define (helper2)
    (let ([f (find-latest-upgrade-number)])
      (if (not f) (if (= sum 0) 0.1 sum)
          (let* ([ft (resource-time-get resource-number curr-mine f particle)]
                 [fu (upgrade-time resource-number curr-mine f)])
            (if (> (+ ft fu) time) (begin (set! curr-mine (+ curr-mine 1))
                                          (helper2))
                (begin (set! sum (+ sum (acc-rate-of-mine resource-number
                                                          curr-mine
                                                          f)))
                       (set! curr-mine (+ curr-mine 1))
                       (helper2)))))))
  (helper2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Shortcut wala resource-amount
;Does not have use-up
;Only to verify the original resource-amount
(define (resource-amount2 resource-number time particle)
  (define curr-time 0)
  (define sum (initial-amount resource-number))
  (define small-time 0.01)
  (define (helper)
    (if (>= curr-time time) sum
        (begin (set! sum (+ sum (* small-time
                                   (acc-rate resource-number curr-time particle))))
               (set! curr-time (+ curr-time small-time))
               (helper))))
  (helper))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resource-amount
         resource-number
         time
         particle)
  (define sum (initial-amount resource-number))
  (define (accumulate)
    (define curr-mine 1)
    (define (go-thru-mine)
      (define curr-level (initial-level resource-number curr-mine))
      (define curr-time 0)
      (define (helper)
        (if (or (> curr-level (total-no-of-upgrades resource-number curr-mine))
                (> curr-time time)) (void)
                                    (let* ([t (resource-time-get resource-number curr-mine (+ curr-level 1) particle)])
                                      (if (> t time) (set! sum (+ sum (* (- time curr-time)
                                                                         (acc-rate-of-mine resource-number curr-mine curr-level))))
                                          (begin (set! sum (+ sum (* (- t curr-time)
                                                                     (acc-rate-of-mine resource-number curr-mine curr-level))))
                                                 (set! curr-time (+ t (upgrade-time resource-number curr-mine (+ curr-level 1))))
                                                 (set! curr-level (+ curr-level 1))
                                                 (helper))))))
      (helper))
    (define (go-thru-resource)
      (if (> curr-mine (no-of-mines resource-number)) (void)
          (begin (go-thru-mine)
                 (set! curr-mine (+ curr-mine 1))
                 (go-thru-resource))))
    (go-thru-resource))
  (define (use-up)
    (define curr-resource 1)
    (define (go-thru-resource)
      (define curr-mine 1)
      (define (go-thru-mine)
        (define curr-level 0)
        (define (helper)
          (if (> curr-level (total-no-of-upgrades curr-resource curr-mine)) (void)
              (if (> (resource-time-get curr-resource curr-mine curr-level particle)
                     time) (void)
                           (let ([cost (vector-ref (upgrade-cost curr-resource curr-mine curr-level) (- resource-number 1))])
                             (set! sum (- sum cost))
                             (set! curr-level (+ curr-level 1))
                             (helper)))))
        (helper))
      (define (helper)
        (if (> curr-mine (no-of-mines curr-resource)) (void)
            (begin (go-thru-mine)
                   (set! curr-mine (+ curr-mine 1))
                   (helper))))
      (helper))
    (define (helper)
      (if (> curr-resource no-of-resources) (void)
          (begin (go-thru-resource)
                 (set! curr-resource (+ curr-resource 1))
                 (helper))))
    (helper))
  (accumulate)
  (use-up)
  sum)

;;;;;;;;;;;;;;;;;;;;;;;
;NOW THE REAL FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;
;They apply PSO
;;;;;;;;;;;;;;;;;;;;;;;

(define (initiate-particles)
  (define (helper n)
    (if (= n no-of-particles) (displayln "Particles initiated")
        (begin
          (vector-set!
           particle-vector n
           (build-vector
            no-of-resources
            (lambda (res-num)
              (build-vector
               (no-of-mines (+ res-num 1))
               (lambda (mine-num)
                 (build-vector
                  (no-of-upgrades (+ res-num 1) (+ mine-num 1)) 
                  (lambda (x)
                    (+ lowerbound
                       (* (random)
                          (- upperbound
                             lowerbound))))))))))
          (helper (+ n 1)))))
  (helper 0))

(define (validate particle)
  (define check-done-vector
    ;initial-state-vector
    (build-vector
     no-of-resources
     (lambda (res-num)
       (build-vector
        ;(no-of-mines (+ res-num 1))
        (vector-ref no-of-mines-vector res-num)
        ;
        (lambda (mine-num)
          (initial-level-of-mines (+ res-num 1) (+ mine-num 1)))))))
  (define (checked-level resource-number mine-number)
    (vector-ref (vector-ref check-done-vector (- resource-number 1)) (- mine-number 1)))
  (define (find-min)
    ;finds the first unchecked upgrade
    ;returns false is all has been checked
    (define min-res 1)
    (define min-mine 1)
    (define curr-min HIGH-VAL)
    (define curr-resource 1)
    (define (go-thru-resource)
      (define curr-mine 1)
      (define (helper)
        (if (> curr-mine (no-of-mines curr-resource)) (void)
            (let ([t (resource-time-get curr-resource curr-mine (+ 1 (checked-level curr-resource curr-mine)) particle)])
              (if (>= t curr-min) (begin (set! curr-mine (+ curr-mine 1))
                                         (helper))
                  (begin (set! curr-min t)
                         (set! min-res curr-resource)
                         (set! min-mine curr-mine)
                         (set! curr-mine (+ curr-mine 1))
                         (helper))))))
      (helper))
    (define (helper)
      (if (> curr-resource no-of-resources) (void)
          (begin (go-thru-resource)
                 (set! curr-resource (+ curr-resource 1))
                 (helper))))
    (helper)
    (if (> curr-min (- HIGH-VAL 5)) #f
        (cons min-res min-mine)))
  (define (validate-1 min-res min-mine);validates a particle and updates check-done accordingly
    (define tm (resource-time-get min-res min-mine (+ 1 (checked-level min-res min-mine)) particle))
    (define tp (resource-time-get min-res min-mine (checked-level min-res min-mine) particle))
    (define tu (upgrade-time min-res min-mine (checked-level min-res min-mine)))
    (define tf (+ tp tu))
    (define (update-check-done)
      (vector-set! (vector-ref check-done-vector (- min-res 1)) (- min-mine 1) (+ 1 (checked-level min-res min-mine))))
    (define (validate-cost);validates cost and updates check-done
      (define cost (upgrade-cost min-res min-mine (+ 1 (checked-level min-res min-mine))))
      (define curr-resource 1)
      (define (helper)
        (if (> curr-resource no-of-resources) (update-check-done)
            (let ([s (- (vector-ref cost (- curr-resource 1)) (resource-amount curr-resource tm particle))])
              (if (<= s 0) (begin (set! curr-resource (+ curr-resource 1))
                                  (helper))
                  (resource-time-set! min-res min-mine (+ 1 (checked-level min-res min-mine)) particle (+ tm 0.1 (/ s
                                                                                                                (acc-rate curr-resource tm particle))))))))
      (helper))
    (if (> tf tm) (resource-time-set! min-res min-mine (+ 1 (checked-level min-res min-mine)) particle tf)
        (validate-cost)))
  (define (helper)
    (let ([d (find-min)])
      (if (not d) (void)
          (begin (validate-1 (car d) (cdr d))
                 (helper)))))
  (helper))
                
        
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;APPLYING THEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(initiate-particles)

;;;;;;
;PSO
;;;;;;
(define c1 0.0025)
(define c2 0.0025)

(define (PSO trials)

  (define max-position particle-vector)

  (define max-values (vector-map fn particle-vector))

  (define velocities (all-zero particle-vector))

  (define (new-gmax)
    (define i 0)
    (define j -2)
    (vector-map (lambda (a) (if (>= gmax a) (set! i (+ 1 i)) (begin (set! gmax a) (set! j i) (set! i (+ 1 i))))) max-values)
    (if (> j -1) (set! gmaxparticle (vector-ref max-position j)) (void)))

  (define gmax (vector-ref max-values 0))

  (define gmaxparticle (vector-ref max-position 0))

  ;(define (index i)
   ; (if (= gmax (vector-ref max-values i)) i (index (+ 1 i))))

  (define (new-positions)
    (set! particle-vector
          (vector-map (lambda (a-p b-p)
                        (vector-map (lambda (a-r b-r)
                                      (vector-map (lambda (a-t b-t)
                                                    (vector-map (lambda (a-v b-v) (+ a-v b-v))
                                                                a-t b-t))
                                                  a-r b-r))
                                    a-p b-p))
                      particle-vector velocities)))
    

  (define (new-max-position-max-value)
    (define i 0)
    (set! max-position
          (vector-map (lambda (particle1 max-position1 max-value1)
                 (let*([particle-val (fn particle1)])
                   (cond[(>= particle-val max-value1) (vector-set! max-values i particle-val)
                                                      (set! i (+ 1 i))
                                                      particle1]
                        [else(set! i (+ 1 i))
                             max-position1])))
               particle-vector max-position max-values)))

  (define (new-velocity)
    (set! velocities
          (vector-map (lambda (a-p b-p c-p)
                        (vector-map (lambda (a-r b-r c-r d-r)
                                      (vector-map (lambda (a-t b-t c-t d-t)
                                                    (vector-map (lambda (a-v b-v c-v d-v) (+ a-v (* c1 (- b-v c-v)) (* c2 (- d-v c-v))))
                                                                a-t b-t c-t d-t))
                                                  a-r b-r c-r d-r))
                                    a-p b-p c-p gmaxparticle))
                      velocities max-position particle-vector)))

  (define (display-all)
;    (displayln "particle vector") (displayln particle-vector)
;    (displayln "max-position") (displayln max-position)
;    (displayln "max-values") (displayln max-values)
;    (displayln "gmax") (displayln gmax)
;    (displayln "velocities") (displayln velocities)
    (void))

  (define (PSO-h trials)
    (new-max-position-max-value) ;(displayln "max-position")(displayln max-position) (displayln "max-values")(displayln max-values)
    (new-gmax) ;(displayln "gmax")(displayln gmax)
    (new-velocity) ;(displayln "velocities") (displayln velocities)
    (new-positions) ;(displayln "particle vector") (displayln particle-vector)
    (validate-all)
    (if (= trials 0) (begin (validate gmaxparticle) gmaxparticle) (PSO-h (- trials 1))))

  (PSO-h trials))


(define (fn particle) (resource-amount 1 100 particle))
;(define (fn particle) )

(define (all-zero vect)
  (vector-map (lambda (a)
                (vector-map (lambda (b)
                              (vector-map (lambda (c)
                                            (make-vector (vector-length c) 0))
                                          b))
                            a))
              vect))

(define (validate-all)
  (define (validator n)
    (if (> n no-of-particles)
        (void)
        (begin (validate (particle n))
               (validator (+ 1 n)))))
  (validator 1))

(define (get-value resource  b c d)
  (vector-ref (vector-ref (vector-ref resource  b) c) d))
  (send secondFrame show #f) (send thirdFrame show #t))
