;; CONCEPT
; this is a tutorial GUI for each function in racket
; the usage of each function will be demonstrated by interactive examples
; the interaction is made mostly with text fields (for entering data in general) and sliders (for numerical arguments)
; the idea is that user will see the result immediately as silder goes or text entered
; the results (evaluation) are printed out as messages on the canvas

;; how to run this
; to get into a programming environment: open terminal
; to load racket program, type in: racket
; after entering racket, to load the file, type in: (load "filename.scm")

;; load the GUI package
(require racket/gui/base)
; and the package that turns messages into pictures
; so we can change their colors
(require pict)

;; SKETCH
#|
 ______________________________________________
|text field has an examples
|(hint message & error message)
|================
| introduce function..
| evaluation of function..
|---------------------------
| .....
|_______________________________________

|#

;; SMALL ISSUES
;; 1. after current data in text field is read, if there is an exception,
;; i catch it by the string "...", after that,
;; i let the callback check the validity of the data in text field by (equal? "..." temp-data).
;; this is not an optimal solution.
;; because "..." is evaluated as #t -> test still passes

;; the biggest canvas
(define big-frame (new frame% [label "vector"]
                       [min-width 500]))

;; SUPPORT FUNCTIONS to read string entered in the text field
(define (read<-data s)
  (read (open-input-string s)))
; read<-data is not able to catch extension (error)
; now we add that ability so it becomes (current-data...)
; because this function will get the data as is from the text fields
(define (current-data a-text-field)
  (with-handlers ([exn:fail? (lambda (v) "...")])
		 (read<-data (send a-text-field get-value))))
; now we add the ability to evaluate the data it reads from the text fields
(define (current-value a-text-field)
  (with-handlers ([exn:fail? (lambda (v) "...")])
		 (eval (read<-data (send a-text-field get-value)))))

;; SUPPORT FUNCTIONS TO DEAL WITH COLORFUL MESSAGES
(define (create-pict-message parent-frame label-string)
  (new message%
       [parent parent-frame]
       [stretchable-width #t]
       [label (pict->bitmap (text label-string))]))
(define (colorize-pict-message message-name label-string color-string)
  (send message-name set-label
	(pict->bitmap
         (colorize
          (text label-string)
          (make-object color% color-string)))))
; deeppink
; royalblue
; mediumturquoise

;; TEMPORARY VALUES
;; we need places to store static values
; like a warehouse storing dried hay for the next crop operation
; that's why these values are called temporary
; some initial temporary values will be set to be used as examples
(define temporary-vector (vector 1 2 3 4 5 6 7 8 9))
(define temporary-position 0)

;; UPDATING PROCESS FOR TEMP VALUES
;; these temporary values will be updated over time by functions
(define (current-vector)
  (current-value vector-field))
(define (current-position)
  (send position-slider get-value))
;; GUI
;; TEXT FIELD
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
		  (update-slider!)         ; the slider is reset
		  (update-ref!))  ; the arguments of vector-length & vector-ref are updated
                (update-error!))))]         ; if the value typed in is not a vector, an error message shows up
       [stretchable-width #t]))

;; HINT & ERROR MESSAGES
(define hint
  (create-pict-message big-frame
     " "))
(colorize-pict-message hint "(hint: add numbers, strings, symbols, nested vectors and lists.. check brackets!)" "RoyalBlue")
(new message% [parent big-frame] [label "------------------"])

;; INPUT & OUTPUT MESSAGES
(define ref-input (new message%
			  [parent big-frame]
			  [stretchable-width #t]
			  [label (~s (list 'vector '...))]))

(define ref-output (new message%
			   [parent big-frame]
			   [stretchable-width #t]
			   [label "=>..."]))
(new message% [parent big-frame] [label ""])

;; SLIDER
;; position pane
(define position-pane (new horizontal-pane%
			 [parent big-frame]
			 [stretchable-width #t]))

; the reason i need a position pane for the position slider is that
; the process of updating the position slider is actually by
; deleting the current one and creating a completely new one.
; that's the only way to reset the slider.
; but each time "updating", the newly created slider will
; be placed automatically at the end of the parental frame it belongs (as new object)
; so it needs a whole parental frame on its own

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
	    (update-ref!))])))
; each time user slides, the position argument in vector-length and vector-ref will be updated in real time

(define position-slider (make-position-slider))
; it's convenient to create a procedure to create the sliders
; and then create a static symbol to store these values

;; END GUI

;; TASK FOR CALLBACKS (sub-function inside each dynamic object: sliders, text-fields..)
; as user types/slides, these callbacks will go to do multiple things
; such as: update the messages, refresh the arguments,
; re-evaluate the functions, print out new results..

;; update the sliders
(define (update-slider!)
  (begin
    (set! temporary-position (current-position))
    (send position-pane delete-child position-slider)
    (set! position-slider (make-position-slider))))

;; update the input & output messages
(define (update-ref!)
  (begin
    (update-ref-input!)
    (update-ref-output!)))

;; error message & hint message
(define (update-error!)
  (colorize-pict-message hint "=> ERROR: not a vector, check brackets!"
                         "DeepPink"))

(define (update-hint!)
  (colorize-pict-message hint "(hint: add numbers, string, symbols, nested vectors.. check brackets)" "RoyalBlue"))

;; smaller tasks inside updating input & output messages
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

;; END TASK
(send big-frame show #t)
