;; load the GUI package, picture package & the master file with some support functions
(require racket/gui/base
         pict
	 "master.scm")

#|
SKETCH
 ______________________________________________
|text field has an exemplary vector
|(hint message & error message)
|================
| introduce function vector-length with the exemplary vector
| => evaluation of function vector-length
|---------------------------
| vector-ref
| a slider to change the argument position in vector-ref function
| => real time evaluation
|--------------------------
| vector-drop
| vector-take
| a slider for the argument of how many to drop & how many to take
| => real time evaluation
|_______________________________________

|#
;; the biggest canvas
(define big-frame (new frame% [label "vector"]
                       [min-width 500]))

;; TEMP VALUES
; initial temporary values are set to be used as examples
(define temporary-vector (vector 1 2 3 4 5 6 7 8 9))

(define temporary-position 0)
(define temporary-drop&take 0)

;; UPDATE TEMP VALUES
;; these temporary values will be updated over time by a function (current-..)
(define (current-vector)
  (current-value vector-field))

(define (current-position)
  (let ([a (send position-slider get-value)])
    (if (< a (- (vector-length temporary-vector) 1))
	a
	(- (vector-length temporary-vector) 1))))

(define (current-drop&take)
  (let ([a (send position-slider get-value)])
    (if (< a (vector-length temporary-vector))
	a
	(vector-length temporary-vector))))

;; GUI
;; VECTOR FIELD
;; a text field for user to enter input (string)
(define vector-field
  (new text-field%
       [parent big-frame]
       [label "enter a vector here:"]
       [init-value "(vector 1 2 3 4 5 6 7 8 9)"]
       [callback
	(lambda (b e)
	  (let ([a (current-vector)]) ; current-vector will get what user typed in and evaluates
	    (if (vector? a)                   ; if that value typed in is a vector
		(begin
		  (update-hint!)            ; general hints will show up
		  (set! temporary-vector a)  ; that vector is set as temp value
		  (update-sliders!)         ; the slider is reset
		  (update-length&ref!)  ; the arguments of vector-length & vector-ref are updated
		  (update-drop&take!)) ; the arguments of vector-drop & vector-take are updated
		(update-error!))))]         ; if the value typed in is not a vector, an error message shows up
       [stretchable-width #t]))

;; ERROR & HINT MESSAGES
(define hint
  (create-pict-message big-frame " "))

(colorize-pict-message
 hint
 "(hint: add numbers, string, symbols, nested vectors, nested lists.. check brackets)"
 "RoyalBlue")

(new message% [parent big-frame] [label "------------------"])

;; INPUT & OUPUT MESSAGES
;; vector-length function
(define length-input (new message%
			  [parent big-frame]
			  [stretchable-width #t]
			  [label (~s (list 'vector-length 'a-vector))]))

(define length-output (new message%
			   [parent big-frame]
			   [stretchable-width #t]
			   [label "=>"]))
(new message% [parent big-frame] [label ""])

;;  vector-ref
(define ref-input (new message%
		   [parent big-frame]
		   [stretchable-width #t]
		   [label (~s (list 'vector-ref 'a-vector 'a-position))]))

(define ref-output (new message%
		    [parent big-frame]
		    [label "=>"]
		    [stretchable-width #t]))

;; SLIDER
;; position pane
(define position-pane (new horizontal-pane%
			 [parent big-frame]
			 [stretchable-width #t]))

;; position slider
(define (make-position-slider)
  (let ([a (- (vector-length temporary-vector) 1)]
	[b temporary-position])
    (new slider%
	 [parent position-pane]
	 [min-value 0]
	 [max-value (if (= a -1) 0 a)]
	 [init-value (if (= b -1) 0 b)]
	 [label "Position"]
	 [callback
	  (lambda (b e)
	    (update-length&ref!))])))
; each time user slides, the position argument in vector-length and vector-ref will be updated

(define position-slider (make-position-slider))

(new message% [parent big-frame] [label ""])

;; OTHER INPUT & OUTPUT MESSAGES
;; vector-drop
(define drop-input (new message%
			[parent big-frame]
			[stretchable-width #t]
			[label (~s (list 'vector-drop 'a-vector 'how-many-to-drop))]))

(define drop-output (new message%
			 [parent big-frame]
			 [stretchable-width #t]
			 [label "=>"]))

;; vector-take
(define take-input (new message%
			[parent big-frame]
			[stretchable-width #t]
			[label (~s (list 'vector-take 'a-vector 'how-many-to-take))]))

(define take-output (new message%
			 [parent big-frame]
			 [stretchable-width #t]
			 [label "=>"]))

;; ANOTHER SLIDER
;; drop and take pane
(define drop&take-pane (new horizontal-pane% [parent big-frame]))
; each slider needs a seperate pane

;; drop and take slider
(define (make-drop&take-slider)
  (let ([a (vector-length temporary-vector)]
	[b temporary-drop&take])
    (new slider%
	 [parent drop&take-pane]
	 [min-value 0]
	 [max-value a]
	 [init-value b]
	 [label "How many?"]
	 [callback
	  (lambda (b e)
	    (update-drop&take!))])))

(define drop&take-slider (make-drop&take-slider))

;; final message
(define msg (new message% [parent big-frame] [label "hi there"]))
;; END GUI

;; CALLBACK TASKS
;; update the sliders
(define (update-sliders!)
  (begin
    (set! temporary-position (current-position))
    (send position-pane delete-child position-slider)
    (set! position-slider (make-position-slider))
    (set! temporary-drop&take (current-drop&take))
    (send drop&take-pane delete-child drop&take-slider)
    (set! drop&take-slider (make-drop&take-slider))))

;; update the messages
(define (update-length&ref!)
  (begin
    (update-ref-input!)
    (update-length-input!)
    (update-ref-output!)
    (update-length-output!)))

(define (update-drop&take!)
  (begin
    (update-drop-input!)
    (update-take-input!)
    (update-drop-output!)
    (update-take-output!)))

;; error message
(define (update-error!)
  (colorize-pict-message
   hint
   "=> ERROR: not a vector, check brackets!"
   "DeepPink"))

(define (update-hint!)
  (colorize-pict-message
   hint
   "(hint: add numbers, string, symbols, nested vectors.. check brackets)"
   "RoyalBlue"))

;; smaller tasks in updating input & output messages
(define (update-ref-input!)
  (send ref-input set-label
	(~s (list
	     'vector-ref
	     temporary-vector
	     (send position-slider get-value)
	     ))))

(define (update-ref-output!)
  (if (equal? #() temporary-vector)
      (send ref-output set-label "=> error: empty vector")
      (send ref-output set-label
	    (~s '=>
		(vector-ref
		 temporary-vector
		 (send position-slider get-value))))))

;; fix vector-length
(define (update-length-input!)
  (send length-input set-label
	(~s (list
	     'vector-length
	     temporary-vector))))

(define (update-length-output!)
  (send length-output set-label
	(~s '=> (vector-length temporary-vector))))

;; task to fix vector-drop
(define (update-drop-input!)
  (send drop-input set-label
	(~s (list
	     'vector-drop
	     temporary-vector
	     (send drop&take-slider get-value)
	     ))))

(define (update-drop-output!)
  (send drop-output set-label
	(~s '=>
	    (vector-drop
	     temporary-vector
	     (send drop&take-slider get-value)))))

;; task to fix vector-take
(define (update-take-input!)
  (send take-input set-label
	(~s (list
	     'vector-take
	     temporary-vector
	     (send drop&take-slider get-value)
	     ))))

(define (update-take-output!)
  (send take-output set-label
	(~s '=>
	    (vector-take
	     temporary-vector
	     (send drop&take-slider get-value)))))

;; END TASK
(send big-frame show #t)
