;; load the GUI package
(require racket/gui/base)
;; load the package for colorful message
(require pict)
;; load the support file
(require "master.scm")

;; the biggest canvas
(define big-frame (new frame% [label "cond"]
		   [min-width 1000]))

;; FOR COND
;; SKETCH
#|

OPERATION PANE
 ---------------------------------                ---------------------------------------------
| ------------------------ |       |             | big pane
||(cond [(...) (...)]  ||       |              | -------------------------------------
||           [(...) (...)]  ||       |              || input pane
||           [(...) (...)]  ||       |   --->     ||    -------------------------------------
| ----------- --------- -- |       |              ||   | cond pane: test-field + action-field  |
|                                |       |             ||    -------------------------------------
|=> ...                        |       |             ||             -> message-hint
 ---------------------------------                ||    -------------------------------------
                                                          ||   |                                       |
                                                          ||    -------------------------------------
                                                          ||             -> message-hint

|#

;; TEMP VALUES
(define temp-test-1 '(zero? 2))
(define temp-act-1 '(list 1))
(define temp-test-2 '(even? 9))
(define temp-act-2 '(sub1 9))
(define temp-act-3 '3)
;; UPDATE TEMP VALUES
(define (current-test-1) (current-data test-field-1))
(define (current-act-1) (current-data act-field-1))
(define (current-test-2) (current-data test-field-2))
(define (current-act-2) (current-data act-field-2))
(define (current-act-3) (current-data act-field-3))

(define (current-test-value-1) (current-value test-field-1))
(define (current-test-value-2) (current-value test-field-2))
(define (current-act-value-1) (current-value act-field-1))
(define (current-act-value-2) (current-value act-field-2))
(define (current-act-value-3) (current-value act-field-3))

;; LIST OF MESSAGES FOR ILLUSTRATION PANE
(define (current-test-1-msg) (~s temp-test-1))
(define (current-test-1-value-msg)
  (if (equal? "..." (current-test-value-1)) (~s '=>...) (~s '=> (eval temp-test-1))))
(define (current-act-1-msg) (~s temp-act-1))
(define (current-act-1-value-msg)
  (if (equal? "..." (current-act-value-1)) (~s '=>...) (~s '=> (eval temp-act-1))))
(define (current-test-2-msg) (~s temp-test-2))
(define (current-test-2-value-msg)
  (if (equal? "..." (current-test-value-2)) (~s '=>...) (~s '=> (eval temp-test-2))))
(define (current-act-2-msg) (~s temp-act-2))
(define (current-act-2-value-msg)
  (if (equal? "..." (current-act-value-2)) (~s '=>...) (~s '=> (eval temp-act-2))))
(define (current-act-3-msg) (~s temp-act-3))
(define (current-act-3-value-msg)
  (if (equal? "..." (current-act-value-3)) (~s '=>...) (~s '=> (eval temp-act-3))))

(define (current-input-msg) (~s (list 'cond (list temp-test-1 temp-act-1)
				      (list temp-test-2 temp-act-2)
				      (list 'else temp-act-3))))

(define (current-output-msg)
  (if (equal? "..." (current-test-value-1))
      "=>..."
      (if (current-test-value-1)
          (current-act-1-value-msg)
          (if (equal? "..." (current-test-value-2))
              "=>..."
              (if (current-test-value-2)
                  (current-act-2-value-msg)
                  (current-act-3-value-msg))))))

;; LIST OF MESSAGES FOR OPERATION PANE
(define operation-messages
  (list  "↑ if this expression is true, then do this action ↑"
         "if none of the above is true, then do this action ↑"
         "↑ ERROR: not a yes/no question"
         "ERROR: not an expression ↑"
         "-> check brackets, number of arguments, expected types of arguments.."))

;; GUI FOR OPERATION PANE OF `COND
;; 3 PANES FOR 6 TEXT-FIELDS
(define cond-pane (new horizontal-pane% [parent big-frame]))
(define cond-input-pane (new vertical-pane% [parent cond-pane]))
(define cond-output-pane (new vertical-pane% [parent cond-pane]))

;; CLAUSE 1
(define cond-clause-1 (new horizontal-pane% [parent cond-input-pane]))
; TEST 1
(define test-field-1 (new text-field% [parent cond-clause-1]
			  [label "(cond [ "]
			  [init-value (current-test-1-msg)]
			  [stretchable-width #t]
			  [callback (lambda (b e)
				      (set! temp-test-1 (current-test-value-1))
				      (if (equal? temp-test-1 "...")
					  (begin
					    (update-error-test-1!)
					    (set! temp-test-1 (current-test-1))
					    (update-input!)
					    )
					  (begin
					    (update-hint-1!)
					    (set! temp-test-1 (current-test-1))
					    (update-input!)
					    ))
				      (update-output!)
				      (f5-illustration-pane))]))

; ERROR & HINT MESSAGES OF TEST 1
(define hint-1 (new horizontal-pane% [parent cond-input-pane]))
(define test-1-msg
  (create-pict-message hint-1 " "))
(colorize-pict-message test-1-msg
 (list-ref operation-messages 0)
 "RoyalBlue")
(define hint-1-msg
  (create-pict-message cond-input-pane  ""))

; ACTION 1
(define act-field-1 (new text-field% [parent cond-clause-1]
			 [label " "]
			 [init-value (current-act-1-msg)]
			 [stretchable-width #t]
			 [callback (lambda (b e)
				      (set! temp-act-1 (current-act-value-1))
				      (if (equal? temp-act-1 "...")
					  (begin
					    (update-error-act-1!)
					    (set! temp-act-1 (current-act-1))
					    (update-input!)
					    )
					  (begin
					    (update-hint-1!)
					    (set! temp-act-1 (current-act-1))
					    (update-input!)
					    ))
				      (f5-illustration-pane)
				      (update-output!))]
			 ))

(new message% [parent cond-clause-1] [label "]"])

;; CLAUSE 2
(define cond-clause-2 (new horizontal-pane% [parent cond-input-pane]))
(new message% [parent cond-clause-2] [label "           "])
; TEST 2
(define test-field-2 (new text-field% [parent cond-clause-2]
			  [label " [ "]
			  [init-value (current-test-2-msg)]
			  [stretchable-width #t]
			   [callback (lambda (b e)
				      (set! temp-test-2 (current-test-value-2))
				      (if (equal? temp-test-2 "...")
					  (begin
					    (update-error-test-2!)
					    (set! temp-test-2 (current-test-2))
					    (update-input!)
					    )
					  (begin
					    (update-hint-2!)
					    (set! temp-test-2 (current-test-2))
					    (update-input!)
					    ))
				      (f5-illustration-pane)
				      (update-output!))]))
; ERROR & HINT MESSAGES FOR TEST 2
(define hint-2 (new horizontal-pane% [parent cond-input-pane]))
(define test-2-msg
  (create-pict-message hint-2 " "))
(colorize-pict-message test-2-msg (list-ref operation-messages 0) "RoyalBlue")
(define hint-2-msg
  (create-pict-message  cond-input-pane " "))
; ACTION 2
(define act-field-2 (new text-field% [parent cond-clause-2]
			 [label " "]
			 [init-value (current-act-2-msg)]
			 [stretchable-width #t]
			  [callback (lambda (b e)
				      (set! temp-act-2 (current-act-value-2))
				      (if (equal? temp-act-2 "...")
					  (begin
					    (update-error-act-2!)
					    (set! temp-act-2 (current-act-2))
					    (update-input!)
					    )
					  (begin
					    (update-hint-2!)
					    (set! temp-act-2 (current-act-2))
					    (update-input!)
					    ))
				      (f5-illustration-pane)
				      (update-output!))]
			 ))
(new message% [parent cond-clause-2] [label "]"])

;; CLAUSE 3
(define cond-clause-3 (new horizontal-pane% [parent cond-input-pane]))
(new message% [parent cond-clause-3]
     [stretchable-width #t]
     [label "  "])
(new message% [parent cond-clause-3]
     [label "   [ else "])
; HINT & ERROR MESSAGES FOR CLAUSE 3
(define hint-3 (new horizontal-pane% [parent cond-input-pane]))
(define test-3-msg
  (create-pict-message hint-3 " "))
(colorize-pict-message test-3-msg (list-ref operation-messages 1) "RoyalBlue")
(define hint-3-msg
  (create-pict-message cond-input-pane " "))
; ACTION 3
(define act-field-3 (new text-field% [parent cond-clause-3]
			 [label " "]
			 [stretchable-width #t]
			 [init-value (current-act-3-msg)]
			  [callback (lambda (b e)
				      (set! temp-act-3 (current-act-value-3))
				      (if (equal? temp-act-3 "...")
					  (begin
					    (update-error-act-3!)
					    (set! temp-act-3 (current-act-3))
					    (update-input!)
					    )
					  (begin
					    (update-hint-3!)
					    (set! temp-act-3 (current-act-3))
					    (update-input!)
					    ))
				      (f5-illustration-pane)
				      (update-output!))]
			 ))
(new message% [parent cond-clause-3] [label "]"])

;; INPUT & OUTPUT MESSAGES
(define input (new message% [parent big-frame]
		   [stretchable-width #t]
		   [label "(cond (true? do-this) (true? do-this) (else do-this))"]))
(define output (new message% [parent big-frame]
		    [label "=> "]
		    [stretchable-width #t]))
;; END GUI FOR OPERATION PANE FOR `COND

;;CALLBACK  TASKS FOR OPERATION PANE
; UPDATE INPUT & OUTPUT MESSAGES
(define (update-input!)
  (send input set-label (current-input-msg)))
(define (update-output!)
  (send output set-label (current-output-msg)))

;; UPDATE ERROR & HINT MESSAGES WHEN USER TYPES WRONG
(define (update-error-test-1!)
  (begin
    (colorize-pict-message test-1-msg (list-ref operation-messages 2) "DeepPink")
    (colorize-pict-message hint-1-msg (list-ref operation-messages 4) "RoyalBlue")))
(define (update-error-act-1!)
   (begin
     (colorize-pict-message test-1-msg (list-ref operation-messages 3) "DeepPink")
     (colorize-pict-message hint-1-msg (list-ref operation-messages 4) "RoyalBlue")))

(define (update-error-test-2!)
  (begin
    (colorize-pict-message test-2-msg (list-ref operation-messages 2) "DeepPink")
    (colorize-pict-message hint-2-msg (list-ref operation-messages 4) "RoyalBlue")))
(define (update-error-act-2!)
  (begin
    (colorize-pict-message test-2-msg (list-ref operation-messages 3) "DeepPink")
    (colorize-pict-message hint-2-msg (list-ref operation-messages 4) "RoyalBlue")))

(define (update-error-act-3!)
  (begin
    (colorize-pict-message test-3-msg (list-ref operation-messages 3) "DeepPink")
    (colorize-pict-message hint-3-msg (list-ref operation-messages 4) "RoyalBlue")))

;; UPDATE HINT MESSAGES BACK WHEN USER TYPES RIGHT
(define (update-hint-1!)
  (begin
    (colorize-pict-message test-1-msg (list-ref operation-messages 0) "RoyalBlue")
    (colorize-pict-message hint-1-msg " " "white")))
(define (update-hint-2!)
  (begin
    (colorize-pict-message test-2-msg (list-ref operation-messages 0) "RoyalBlue")
    (colorize-pict-message hint-2-msg " " "white")))
(define (update-hint-3!)
  (begin
    (colorize-pict-message test-3-msg (list-ref operation-messages 1) "RoyalBlue")
    (colorize-pict-message hint-3-msg " " "white")))

;; END FOR OPERATION PANE FOR `COND

;; GUI for ILLUSTRATION PANE
; SLIDER
(define time-slider (new slider%
			 [parent cond-output-pane]
			 [label "step"]
			 [min-value 0]
			 [max-value 5]
			 [init-value 0]
			 [callback
			  (lambda (b e)
			    (let ([a (send time-slider get-value)])
			      (cond [(= 0 a) (step-1)]
				    [(= 1 a) (step-2)]
				    [(= 2 a) (step-3)]
				    [(= 3 a) (step-4)]
				    [(= 4 a) (step-5)]
				    [else (step-6)])))]))
;; WORKFLOW BLOCKS
; TOP SHELF
(define cond-shelf (new horizontal-pane%
			[parent cond-output-pane]))
(new pane%
     [parent cond-shelf])

(define cond (new message%
		  [parent cond-shelf]
		  [stretchable-width #t]
		  [label ""]))

(new pane%
     [parent cond-shelf])

; ARROW SHELF
(define arrow-shelf (new horizontal-pane% [parent cond-output-pane]))
(new pane% [parent arrow-shelf])
(define left-arrow (new message%
			[parent arrow-shelf]
			[stretchable-width #t]
			[label ""]))

(define middle-arrow (new message%
			[parent arrow-shelf]
			[stretchable-width #t]
			[label ""]))

(define right-arrow (new message%
			[parent arrow-shelf]
			[stretchable-width #t]
			[label ""]))
(new pane% [parent arrow-shelf])

; TEST SHELF
(define test-shelf (new horizontal-pane% [parent cond-output-pane]))

(define test-1 (new message%
		    [parent test-shelf]
		    [stretchable-width #t]
		    [label " "]))
(define test-2 (new message%
		    [parent test-shelf]
		    [stretchable-width #t]
		    [label " "]))
(define test-3 (new message%
		    [parent test-shelf]
		    [stretchable-width #t]
		    [label " "]))

(define value-shelf (new horizontal-pane% [parent cond-output-pane]))
(define value-1 (new message%
		    [parent value-shelf]
		    [stretchable-width #t]
		    [label (pict->bitmap (text " "))]))
(define value-2 (new message%
		    [parent value-shelf]
		    [stretchable-width #t]
		    [label (pict->bitmap (text " "))]))
(define value-3 (new message%
		    [parent value-shelf]
		    [stretchable-width #t]
		    [label (pict->bitmap (text " "))]))

; ARROW SHELF
(define arrow-shelf-2 (new horizontal-pane% [parent cond-output-pane]))
(new pane% [parent arrow-shelf-2])
(define left-arrow-2 (new message%
		    [parent arrow-shelf-2]
		    [stretchable-width #t]
		    [label " "]))

(define middle-arrow-2 (new message%
		    [parent arrow-shelf-2]
		    [stretchable-width #t]
		    [label " "]))

(define right-arrow-2 (new message%
		    [parent arrow-shelf-2]
		    [stretchable-width #t]
		    [label " "]))
(new pane% [parent arrow-shelf-2])

; ACTION SHELF
(define action-shelf (new horizontal-pane% [parent cond-output-pane]))

(define action-1 (new message%
		    [parent action-shelf]
		    [stretchable-width #t]
		    [label " "]))
(define action-2 (new message%
		    [parent action-shelf]
		    [stretchable-width #t]
		    [label " "]))
(define action-3 (new message%
		    [parent action-shelf]
		    [stretchable-width #t]
		    [label " "]))

(define output-shelf (new horizontal-pane% [parent cond-output-pane]))
(define output-1 (new message%
		    [parent output-shelf]
		    [stretchable-width #t]
		    [label (pict->bitmap (text " "))]))
(define output-2 (new message%
		    [parent output-shelf]
		    [stretchable-width #t]
		    [label (pict->bitmap (text " "))]))
(define output-3 (new message%
		    [parent output-shelf]
		    [stretchable-width #t]
		    [label (pict->bitmap (text " "))]))

;; END GUI FOR ILLUSTRATION PANE OF `COND

;; CALLBACK TASKS FOR ILLUSTRATION PANE
; MESSAGE LIST
(define message-list
  (list cond
	left-arrow middle-arrow right-arrow
	test-1 test-2 test-3
	value-1 value-2 value-3
	left-arrow-2 middle-arrow-2 right-arrow-2
	action-1 action-2 action-3
	output-1 output-2 output-3
	))

(define label-list
  (list "which test is true?"
	"↙" "↓" "↘"
	'(current-test-1-msg) '(current-test-2-msg) "else"
	'(current-test-1-value-msg) '(current-test-2-value-msg) "=> #t"
	"↓" "↓" "↓"
	'(current-act-1-msg) '(current-act-2-msg) '(current-act-3-msg)
 	'(current-act-1-value-msg) '(current-act-2-value-msg) '(current-act-3-value-msg)
	'(current-act-1-value-msg) '(current-act-2-value-msg) '(current-act-3-value-msg)))

(define position-list
  (list 0
	1 2 3
	4 5 6
	7 8 9
	10 11 12
	13 14 15
	16 17 18))

(define pict-posn-list
  (list 7 8 9 16 17 18))

; WHITE OUT
(define (white-message a-position)
  (if (member a-position pict-posn-list)
      (white-message-at a-position message-list)
      (send (list-ref message-list a-position) set-label "")))

(define (white-messages-but a-list)
  (map white-message (remove* a-list position-list)))

(define (white-messages a-list)
  (map white-message a-list))

; COLOR MESSAGES
(define (color-message a-position a-color)
  (color-message-at a-position a-color message-list label-list))

(define (choose-message-color-at a-posn)
  (color-message
   a-posn
   (if (or
        (equal? "=>..." (eval (list-ref label-list a-posn)))
        (equal? "=> #f" (eval (list-ref label-list a-posn))))
       "DeepPink"  "RoyalBlue")))

(define (color-messages)
  (map choose-message-color-at pict-posn-list))

(define (set-label-at a-position)
  (send (list-ref message-list a-position)
	set-label (eval (list-ref label-list a-position))))
(define (set-all-labels!)
  (begin
    (map set-label-at (remove* pict-posn-list position-list))
    (color-messages)))

; COLOR FIELDS
(define field-list
  (list test-field-1 act-field-1
	test-field-2 act-field-2
	act-field-3))

(define field-posn-list
  (list 0 1
	2 3
	  4))

(define value-at-field
  (list '(current-test-value-1)
        '(current-act-value-1)
        '(current-test-value-2)
        '(current-act-value-2)
        '(current-act-value-3)))

(define (color-field-at a-position a-color)
  (send (list-ref field-list a-position) set-field-background
        (make-object color% a-color)))

(define (white-all-fields)
  (map (lambda (x) (color-field-at x "white"))
       field-posn-list))

(define (choose-field-color-at a-posn)
  (begin
    (white-all-fields)
    (color-field-at a-posn
                    (if (equal? "..." (eval (list-ref value-at-field a-posn)))
                        "LightCoral" "MediumTurquoise"))))

(define (f5-illustration-pane)
  (begin
    (send time-slider set-value 0)
    (white-messages-but '())
    (white-all-fields)))

;; WORKFLOW CORRESPONDING TO THE SLIDER
(define (step-1)
  (begin
    (set-all-labels!)
    (white-messages-but '())
    (white-all-fields)))

(define (step-2)
  (begin
     (set-all-labels!)
    (white-messages-but '(0 1 2 3 4 5 6))
    (white-all-fields)))

(define (step-3)
  (begin
    (set-all-labels!)
    (white-messages '(10 11 12 13 14 15 16 17 18))
    (if (equal? "..." (current-test-value-1))
        (choose-field-color-at 0)
       (if  (equal? "..." (current-test-value-2))
            (choose-field-color-at 2)
	    (white-all-fields)))))

(define (step-4)
    (begin
      (set-all-labels!)
      (if (equal? "..." (current-test-value-1))
          (choose-field-color-at 0)
          (if (eval temp-test-1)
              (begin
                (white-messages-but '(0 1 4 7))
                (choose-field-color-at 0))
              (if (equal? "..." (current-test-value-2))
                  (choose-field-color-at 2)
                  (if (eval temp-test-2)
                      (begin
                        (white-messages-but '(0 2 5 8))
                        (choose-field-color-at 2))
                      (begin
                        (white-messages-but '(0 3 6 9))
                        (white-all-fields))))))))


(define (step-5)
  (if (equal? "=>..." (current-output-msg))
      (white-messages-but '())
      (begin
        (set-all-labels!)
        (if (equal? "..." (current-test-value-1))
            (choose-field-color-at 0)
            (if (eval temp-test-1)
                (begin
                  (white-messages-but '(0 1 4 7 10 13))
                  (choose-field-color-at 1))
                (if (equal? "..." (current-test-value-2))
                    (choose-field-color-at 2)
                    (if (eval temp-test-2)
                        (begin
                          (white-messages-but '(0 2 5 8 11 14))
                          (choose-field-color-at 3))
                        (begin
                          (white-messages-but '(0 3 6 9 12 15))
                          (choose-field-color-at 4)))))))))

(define (step-6)
  (if (equal? "=>..." (current-output-msg))
      (white-messages-but '())
      (begin
        (set-all-labels!)
        (if (eval temp-test-1)
            (begin
              (white-messages-but '(0 1 4 7 10 13 16))
              (choose-field-color-at 1))
            (if (eval temp-test-2)
                (begin
                  (white-messages-but '(0 2 5 8 11 14 17))
                  (choose-field-color-at 3))
                (begin
                  (white-messages-but '(0 3 6 9 12 15 18))
                  (choose-field-color-at 4)))))))

;; END FOR COND

;; AND
;; TEMP VALUES
(define temp-input '(and 9 #f #t))
;; UPDATE TEMP VALUES
(define (current-input) (current-data and-field))
;; GUI
(new message% [parent big-frame]
     [label "----------------------------------------------------------------------------------------------------- "])
(define and-input-pane (new vertical-pane% [parent big-frame]
			    [stretchable-width #t]))

(define and-pane (new horizontal-pane% [parent and-input-pane]))
; TEXT FIELD
(define and-field (new text-field% [parent and-pane]
		       [label " "]
		       [init-value "(and 9 #f #t)"]
		       [stretchable-width #t]
		       [callback (lambda (b e)
				   (set! temp-input (current-value and-field))
				   (if (equal? temp-input "...")
				       (begin
					 (update-and-error!)
					 (set! temp-input (current-data and-field))
					 (update-and-input!)
					 (update-and-output!))
				       (begin
					 (set! temp-input (current-data and-field))
					 (update-and-hint!)
					 (update-and-input!)
					 (update-and-output!))))]))

; ERROR & HINT
(define and-hint
  (create-pict-message and-input-pane ""))
(colorize-pict-message and-hint
                       "`and returns the last true argument when all arguments are true, when there is one false argument it returns #f"
                       "royalblue")

(define and-error
  (create-pict-message and-pane " "))

; INPUT & OUTPUT
(define and-input (new message% [parent and-input-pane]
		       [stretchable-width #t]
		       [label (~s temp-input)]))

(define and-output (new message% [parent and-input-pane]
			[stretchable-width #t]
			[label "=> "]))

;; CALLBACK TASKS
(define (update-and-error!)
  (begin
    (colorize-pict-message and-error "<- ERROR: not an expression" "deeppink")
    (colorize-pict-message and-hint "-> check brackets, number of arguments, type of arguments.." "royalblue")))

(define (update-and-hint!)
  (begin
    (colorize-pict-message and-error " " "white")
    (colorize-pict-message and-hint "`and returns the last true argument when all arguments are true, when there is one false argument it returns #f" "royalblue")))

(define (update-and-input!)
  (send and-input set-label (~s temp-input)))

(define (update-and-output!)
  (send and-output set-label (~s '=> (eval temp-input))))
;; END FOR `AND

;; OR
; TEMP VALUES
(define or-temp-input '(or #f #f #t 8 7))
; UPDATE TEMP VALUES
(define (or-current-input) (current-data and-field))
;; GUI
(new message% [parent big-frame] [label " "])
(define or-input-pane (new vertical-pane% [parent big-frame]
			    [stretchable-width #t]))

(define or-pane (new horizontal-pane% [parent or-input-pane]))
; FIELD
(define or-field (new text-field% [parent or-pane]
		       [label " "]
		       [init-value "(or #f #f #t 8 7)"]
		       [stretchable-width #t]
		       [callback (lambda (b e)
				   (set! or-temp-input (current-value or-field))
				   (if (equal? or-temp-input "...")
				       (begin
					 (update-or-error!)
					 (set! or-temp-input (current-data or-field))
					 (update-or-input!)
					 (update-or-output!))
				       (begin
					 (set! or-temp-input (current-data or-field))
					 (update-or-hint!)
					 (update-or-input!)
					 (update-or-output!))))]))
; HINT & ERROR
(define or-hint
  (create-pict-message or-input-pane " "))
(colorize-pict-message or-hint "`or returns the first true argument or it returns #f when all arguments are false" "royalblue")

(define or-error
  (create-pict-message or-pane " "))

; INPUT & OUTPUT
(define or-input (new message% [parent or-input-pane]
		       [stretchable-width #t]
		       [label (~s or-temp-input)]))

(define or-output (new message% [parent or-input-pane]
			[stretchable-width #t]
			[label "=> "]))
; END GUI FOR `OR

;; CALLBACK TASKS
(define (update-or-error!)
  (begin
    (colorize-pict-message or-error "<- ERROR: not an expression" "deeppink")
    (colorize-pict-message or-hint "-> check brackets, number of arguments, type of arguments.." "royalblue")))

(define (update-or-hint!)
  (begin
    (colorize-pict-message or-error " " "White")
    (colorize-pict-message or-hint "`or returns the first true argument or it returns #f when all arguments are false" "royalblue")))

(define (update-or-input!)
  (send or-input set-label (~s or-temp-input)))

(define (update-or-output!)
  (send or-output set-label (~s '=> (eval or-temp-input))))
;; END

(new message% [parent big-frame] [label "hi there"])
(send big-frame show #t)
