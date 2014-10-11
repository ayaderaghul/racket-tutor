;; load the GUI package
(require racket/gui/base
         "master.scm")

#|
SKETCH
 ______________________________________________
|two text fields for two vectors ready to be appended
|(hint message & error message)
|---------------------
| introduce function vector-append with the exemplary vectors
| => real time evaluation of vector-append
|==============================
| to get inputs for vector-set!, there will be 2 text fields for two arguments (the vector and the value to be set)
| (hint message & error message)
| a slider to change the position argument of vector-set!
|---------------------
| introduce the function vector-set! here
| the evaluation of this function will always be #void
|  because it only modifies the old vector
| but the modified vector will be shown here for demonstration
|______________________________

|#

;; the biggest canvas
(define big-frame (new frame%
		   [label "vector-2"]
                   [min-width 620]))

;; TEMP VALUES
; initial temporary values are set to be used as examples
(define temp-vector-1 (vector ))
(define temp-vector-2 (vector ))

;; UPDATE TEMP VALUES
; these temp values will be updated over time by a function (current-value..)
(define (current-vector-1) (current-value vector-field-1))
(define (current-vector-2) (current-value vector-field-2))

;; GUI
;; FOR VECTOR-APPEND
;; VECTOR FIELDS
;; pane for 2 vector fields
(define append-pane (new horizontal-pane%
			 [parent big-frame]))

;; 2 vector fields
(define vector-field-1
  (new text-field%
       [parent append-pane]
       [label "enter vectors here:"]
       [init-value "(vector )"]
       [stretchable-width #t]
       [callback
	(lambda (b e)
	  (let ([a (current-vector-1)])
	    (if (vector? a)
		(begin
		  (append:update-hint!)
		  (set! temp-vector-1 a))
		(begin
                  (append:update-error!)
                  (set! temp-vector-1 (vector "..."))))
            (update-append!)))]))

(define vector-field-2
  (new text-field%
       [parent append-pane]
       [label ""]
       [init-value "(vector )"]
       [stretchable-width #t]
       [callback
	(lambda (b e)
	  (let ([c (current-vector-2)])
	    (if (vector? c)
		(begin
		  (append:update-hint!)
		  (set! temp-vector-2 c))
                (begin
                  (append:update-error!)
                  (set! temp-vector-2 (vector "..."))))
            (update-append!)))]))

;; HINT & ERROR MESSAGES
(define append:hint
  (create-pict-message
   big-frame " "))

(colorize-pict-message
 append:hint
 "(hint: to append 3 vectors, enter in the 1st field: #(1), enter in the 2nd field: (vector-append #(2) #(3)))"
 "RoyalBlue")

;; INPUT & OUTPUT MESSAGES
;; vector-append: input & output messages
(define append:input (new message%
			  [parent big-frame]
			  [stretchable-width #t]
			  [label (~s (list 'vector-append
					   'vector-1
					   'vector-2
					   'vector-3))]))

(define append:output (new message%
			   [parent big-frame]
			   [stretchable-width #t]
			   [label "=> an appended vector"]))

(new message% [parent big-frame] [label "--------------------------------------"])
(new message% [parent big-frame] [label ""])
;; END GUI for vector-append

;; TASK FOR VECTOR-APPEND CALLBACKS
;; update the messages of input & output
(define (update-append!)
  (begin
    (update-append-input!)
    (update-append-output!)))

(define (update-append-input!)
  (send append:input set-label
	(~s (list
	     'vector-append
	     temp-vector-1
	     temp-vector-2))))

(define (update-append-output!)
  (send append:output set-label
	(~s '=>
	    (vector-append
	     temp-vector-1
	     temp-vector-2))))

;; update error message
(define (append:update-error!)
  (colorize-pict-message
   append:hint
   "ERROR: not a vector, check brackets!"
   "DeepPink"))

(define (append:update-hint!)
  (colorize-pict-message
   append:hint
   "(hint: to append 3 vectors, enter in the 1st field: #(1), enter in the 2nd field: (vector-append #(2) #(3)))"
   "RoyalBlue"))

;; END TASK
;; END FOR APPEND

;; FOR VECTOR-SET!
;; TEMP VALUES
(define temp-vector (vector 1 2 3 4 5))
(define temp-set-value 9) ; this is the value to be set into the vector
(define temp-position 0)

;; UPDATE TEMP VALUES
(define (current-vector) (current-value vector-field))
(define (current-set-value) (current-value value-field))
(define (current-position) (send position-slider get-value))

;; FIELDS
;; pane to hold two fields:
(define set!-pane (new horizontal-pane%
		      [parent big-frame]))

;; vector field and value field
(define vector-field (new text-field%
			  [parent set!-pane]
			  [stretchable-width #t]
			  [label "enter a vector here: "]
			  [init-value "(vector 1 2 3 4 5)"]
			  [callback
			   (lambda (b e)
			     (let ([a (current-vector)])
			       (if (vector? a)
				   (begin
				     (set!:update-hint!)
				     (set! temp-vector a))
                                   (begin
                                     (set!:update-vector-error!)
                                     (set! temp-vector (vector "..."))))
                               (update-position-slider!)
                               (update-set!)))]))

(define value-field (new text-field%
			 [parent set!-pane]
			 [stretchable-width #t]
			 [label "enter a value here:"]
			 [init-value "9"]
			 [callback
			  (lambda (b e)
			    (let ([c (current-set-value)])
			      (if (or
                                   (equal? c "...")
                                   (eof-object? c))
                                  (begin
                                    (set!:update-value-error!)
                                    (set! temp-set-value "..."))
				  (begin
				    (set!:update-hint!)
				    (set! temp-set-value c)))
                              (update-position-slider!)
                              (update-set!)))]))

;; HINT & ERROR MESSAGES
(define set!:hint
  (create-pict-message
   big-frame " "))

(colorize-pict-message
 set!:hint
 "(hint: a-value can be number, string, symbol, nested vector..)"
 "RoyalBlue")

;; SLIDER
;; pane for position slider
(define position-pane (new horizontal-pane%
			   [parent big-frame]
			   [stretchable-width #t]))

;; position slider
(define (make-position-slider)
  (let ([a (- (vector-length temp-vector) 1)]
	[b temp-position])
    (new slider%
	 [parent position-pane]
	 [min-value 0]
	 [max-value (if (< a 1) 1 a)]
	 [init-value (if (< b 0) 0 b)]
	 [label "Position"]
	 [callback
	  (lambda (b e)
	    (set! temp-position (current-position))
            (set! temp-vector (current-vector))
; each time user slides, the temp-vector needs to be updated to be back
; to the vector entered in the vector field. if we don't do that, the temp-vector
; stays to be the modified vector
            (update-set!)
	    )])))

(define position-slider (make-position-slider))

;; INPUT & OUPUT MESSAGES
;; vector-set!
(define set!:input (new message%
		       [parent big-frame]
		       [stretchable-width #t]
		       [label (~s (list 'vector-set!
					'a-vector
					'a-position
					'a-value))]))

(new message% [parent big-frame]
     [label "=> this function will always return #void but the modified vector is:"]
     [stretchable-width #t])

(define set!:output (new message%
			[parent big-frame]
			[stretchable-width #t]
			[label "..."]))

;; CALLBACK TASKS
;; slider
(define (update-position-slider!)
  (begin
    (send position-pane delete-child position-slider)
    (set! position-slider (make-position-slider))))

;; update hint & error messages
(define (set!:update-vector-error!)
  (colorize-pict-message
   set!:hint
   "ERROR: not a vector, check brackets!"
   "DeepPink"))

(define (set!:update-value-error!)
  (colorize-pict-message
   set!:hint
   "ERROR: invalid value"
   "DeepPink"))

(define (set!:update-hint!)
  (colorize-pict-message
   set!:hint
   "(hint: a-value can be number, string, symbol, nested vector..)"
   "RoyalBlue"))

;; update set input & output
(define (update-set!)
  (update-set-input!)
  (update-set-output!))

(define (update-set-input!)
  (if
   (equal? #("...") temp-vector)
   (send set!:input set-label
         (~s (list 'vector-set! "..." temp-position temp-set-value)))
   (send set!:input set-label
         (~s (list 'vector-set!
                   temp-vector
                   temp-position
                   temp-set-value)))))

(define (update-set-output!)
  (cond
    [(equal? #("...") temp-vector)
     (send set!:output set-label "...")]
    [(equal? "..." temp-set-value)
     (send set!:output set-label "...")]
    [else
     (begin (vector-set! temp-vector temp-position temp-set-value)
            (send set!:output set-label (~s temp-vector)))]))

;; END FOR VECTOR-SET!
;; END GUI

(send big-frame show #t)
