;; load the GUI  package
;; and the package that turns messages into picture (ie with color)
(require racket/gui/base
         "master.scm")
(require pict)

;; SKETCH
; on the right, the workflow will be shown step by step by sliding the slider
#|
OPERATION PANE                             ILLUSTRATION PANE
 -------------------------------               ---------------------------------------------
| --------- ----------- |       |             | big pane
||(if ...  |<- ...     ||       |             | -------------------------------------
||    ...  |<- ...     ||       |             || input pane
||    ...) |<- ...     ||       |   --->    ||    -------------------------------------
| --------- ------- --- |       |             ||   | if pane: test-field + test-message  |
|          OK            |       |              ||    -------------------------------------
|=> ...                   |       |              ||             -> test-message-hint
 -------------------------------              ||    -------------------------------------
                                                      ||   | pass-pane : pass-field + pass-message |
                                                      ||    -------------------------------------
                                                      ||             -> pass-message-hint
                                                      || ........
|#


;; TEMP data
(define temp-test '(symbol? "a"))
(define temp-pass '(add1 3))
(define temp-fail '(vector "unicorn"))
;; update temp data
(define (current-test) (current-data test-field))
(define (current-pass) (current-data pass-field))
(define (current-fail) (current-data fail-field))

(define (current-test-value) (current-value test-field))
(define (current-pass-value) (current-value pass-field))
(define (current-fail-value) (current-value fail-field))

;; list of messages on the illustration pane (the right pane)
(define (current-test-msg) (~s temp-test))
(define (current-test-value-msg)
  (if (equal? "..." (current-test-value))
      (~s '=>...)
      (~s '=> (eval temp-test))))
(define (current-pass-msg) (~s temp-pass))
(define (current-pass-value-msg)
  (if (equal? "..." (current-pass-value))
      (~s '=>...)
      (~s '=> (eval temp-pass))))
(define (current-fail-msg) (~s temp-fail))
(define (current-fail-value-msg)
  (if (equal? "..." (current-fail-value))
      (~s '=>...)
      (~s '=> (eval temp-fail))))

(define (current-input-msg)
  (~s (list 'if temp-test temp-pass temp-fail)))
(define (current-output-msg)
  (if (or
       (equal? "..." (current-test-value))
       (equal? "..." (current-pass-value))
       (equal? "..." (current-fail-value)))
      (~s '=>...)
      (~s '=> (if (eval temp-test)
                  (eval temp-pass)
                  (eval temp-fail)))))

;; GUI
;; the biggest canvas
(define big-frame (new frame% [label "if"]
                   [min-width 1000]))
;; OPERATION PANE
;; 3 pane for 3 text fields and their messages
(define big-pane
  (new horizontal-pane% [parent big-frame] [stretchable-width #t]))
(define input-pane
  (new vertical-pane% [parent big-pane] [stretchable-width #t]))
(define output-pane
  (new vertical-pane% [parent big-pane] [stretchable-width #t]))

(define if-pane
  (new horizontal-pane% [parent input-pane] [stretchable-width #t]))

(define test-message-hint
  (create-pict-message  input-pane ""))

(define pass-pane
  (new horizontal-pane% [parent input-pane] [stretchable-width #t]))

(define pass-message-hint
  (create-pict-message input-pane  ""))

(define fail-pane
  (new horizontal-pane% [parent input-pane] [stretchable-width #t]))

(define fail-message-hint
  (create-pict-message input-pane  ""))

;; 3 TEXT FIELDS
(define test-field
  (new text-field%
       [parent if-pane]
       [label "(if "]
       [init-value (current-test-msg)]
       [stretchable-width #t]
       [callback (lambda (b e)
                   (set! temp-test (current-test-value))
                   (if (equal? temp-test "...")
                       (begin
                         (test-update-error!)
                         (set! temp-test (current-test))
                         (update-input!)
                         )
                       (begin
                         (test-update-hint!)
                         (set! temp-test (current-test))
                         (update-input!)))
                   (update-output!)
                   (f5-illustration-pane)
                   )]
       ))

(define test-message
  (create-pict-message  if-pane " "))

(colorize-pict-message test-message
                        "<- if this expression is true"
                        "RoyalBlue")

(define pass-field
  (new text-field%
       [parent pass-pane]
       [label ""]
       [init-value (current-pass-msg)]
       [stretchable-width #t]
       [callback (lambda (b e)
                   (set! temp-pass (current-pass-value))
                   (if (equal? temp-pass "...")
                       (begin
                         (pass-update-error!)
                         (set! temp-pass (current-pass))
                         (update-input!))
                       (begin
                         (pass-update-hint!)
                         (set! temp-pass (current-pass))
                         (update-input!)))
                   (update-output!)
                   (f5-illustration-pane)
                   )]))

(define pass-message
  (create-pict-message pass-pane
 " "))

(colorize-pict-message pass-message
                       "<- do this"
                       "RoyalBlue")

(define fail-field
  (new text-field%
       [parent fail-pane]
       [label ""]
       [init-value (current-fail-msg)]
       [stretchable-width #t]
       [callback (lambda (b e)
                   (set! temp-fail (current-fail-value))
                   (if (equal? temp-fail "...")
                       (begin
                         (fail-update-error!)
                         (set! temp-fail (current-fail))
                         (update-input!))
                       (begin
                         (fail-update-hint!)
                         (set! temp-fail (current-fail))
                         (update-input!)))
                   (update-output!)
                   (f5-illustration-pane))]))
(new message%
       [parent fail-pane]
       [label ")"])

(define fail-message
  (create-pict-message  fail-pane
                        " "))

(colorize-pict-message fail-message
                       "<- else, do this"
                       "RoyalBlue")
;; INPUT & OUTPUT MESSAGES
(define input
  (new message%
       [parent big-frame]
       [stretchable-width #t]
       [label (~s (list 'if 'test-passed 'do-this 'else-do-this))]))

(define output
  (new message%
       [parent big-frame]
       [stretchable-width #t]
       [label "=>"]))

;; END GUI FOR THE OPERATION PANE

;; CALLBACK TASKS (for the operation pane)
;; UPDATE ERROR & HINT MESSAGES when user types in the text fields
; if there is error in the text field, the error message needs to be shown
(define (test-update-error!)
  (begin
    (colorize-pict-message
     test-message
     "<- ERROR: not a yes/no question"
     "DeepPink")
    (colorize-pict-message
     test-message-hint
     "-> check brackets, number of arguments, expected types of arguments.."
     "RoyalBlue")
   ))
; after the data is corrected, the hint message needs to be shown
; and the error message needs to be hidden
(define (test-update-hint!)
  (begin
    (colorize-pict-message test-message
          "<- if this expression is true"
          "RoyalBlue")
    (colorize-pict-message test-message-hint " " "white")
   ))

(define (pass-update-error!)
  (begin
    (colorize-pict-message pass-message
                           "<- ERROR: not an expression"
                           "DeepPink")
    (colorize-pict-message
     pass-message-hint
     "-> check brackets, number of arguments, expected types of arguments.."
     "RoyalBlue")
    ))

(define (pass-update-hint!)
  (begin
    (colorize-pict-message pass-message
          "<- do this" "RoyalBlue")
    (colorize-pict-message pass-message-hint " " "white")
    ))

(define (fail-update-error!)
  (begin
    (colorize-pict-message fail-message
          "<- ERROR: not an expression" "DeepPink")
    (colorize-pict-message
     fail-message-hint
     "-> check brackets, number of arguments, expected types of arguments.."
     "RoyalBlue")
    ))
(define (fail-update-hint!)
  (begin
    (colorize-pict-message fail-message
                           "<- else, do this" "RoyalBlue")
    (colorize-pict-message fail-message-hint " " "white")
    ))
; UPDATE INPUT & OUTPUT MESSAGES
(define (update-input!)
  (send input set-label
        (current-input-msg)))

; output
(define (update-output!)
  (send output set-label
        (current-output-msg)))

;; END OPERATION PANE

;; ILLUSTRATION PANE
; this is to the right of the biggest canvas
; on top there is the slider, then the detailed procedure
; the procedure is divided into 3 blocks forming a triangle
; the test block, the pass block and the fail block (plus the arrow decoration)
;; GUI
;; SLIDER
(define slider-pane (new horizontal-pane% [parent output-pane]))
(define time-slider (new slider%
                         [parent slider-pane]
                         [label "step"]
                         [min-value 0]
                         [max-value 4]
                         [init-value 0]
                         [callback
                          (lambda (b e)
                            (let ([a (send time-slider get-value)])
                              (cond [(= 0 a) (step-0)]
                                    [(= 1 a) (step-1)]
                                    [(= 2 a) (step-2)]
                                    [(= 3 a) (step-3)]
                                    [else (step-4)])))]))

;; 3 BLOCKS OF TEST, IF-TRUE, IF-FALSE
(define test-pane (new horizontal-panel%
                        [parent output-pane]))
(new vertical-panel%
      [parent test-pane])

;; TEST BLOCK
(define test-panel (new vertical-panel%
                        [parent test-pane]))
(new vertical-panel%
     [parent test-pane])
(define test
  (create-pict-message test-panel " "))
(define test-input
  (create-pict-message test-panel " "))
(define test-output
  (create-pict-message test-panel " "))

;; ARROW BLOCK
(define arrow-panel (new horizontal-panel% [parent output-pane]))
(new pane% [parent arrow-panel])
(define left-arrow
  (create-pict-message arrow-panel " "))
(new message%  [parent arrow-panel] [label "            "])
(define right-arrow   (create-pict-message arrow-panel " "))
(new pane% [parent arrow-panel])

;; IF-TRUE, IF-FALSE BLOCK
(define action-panel (new horizontal-panel%
                          [parent output-pane]
                          ))
(define pass-panel (new vertical-panel%
                        [parent action-panel]))
(define pass-input   (create-pict-message pass-panel " "))
(define pass-output   (create-pict-message pass-panel " "))
(define fail-panel (new vertical-panel%
                        [parent action-panel]))
(define fail-input   (create-pict-message fail-panel " "))
(define fail-output   (create-pict-message fail-panel " "))
;; END GUI FOR THE ILLUSTRATION PANE

;; CALLBACK TASKS (of the illustration pane)
; all the messages on screen need to be collected for later management
; this is the collection of message (store place)
(define message-list
  (list test test-input test-output
        left-arrow pass-input pass-output
        right-arrow fail-input fail-output
        ))

; the message values to be shown are fixed and listed below
; these values will be send to the above store place in order
(define label-list
  (list
        "evaluate this:"
        '(current-test-msg)
        '(current-test-value-msg)
        "#t ↙"
        '(current-pass-msg)
        '(current-pass-value-msg)
        "↘ #f"
        '(current-fail-msg)
        '(current-fail-value-msg)))

(define position-list
  (list 0 1 2 3 4 5 6 7 8))

;; WHITE OUT
(define (white-message  a-position)
  (white-message-at a-position message-list))

(define (white-messages-but a-list)
	(map white-message  (remove* a-list position-list)))

(define (white-messages a-list)
  (map white-message  a-list))

;; WORK WITH PICTURE MESSAGES
; change color of a message
(define (color-message a-posn a-color)
  (color-message-at a-posn a-color message-list label-list))

(define (choose-message-color-at a-posn)
	(color-message a-posn 
	(if (equal? "=>..." (eval (list-ref label-list a-posn)))
		"DeepPink" "RoyalBlue")))

(define (color-all-messages)
  (begin
    (map (lambda (y) (color-message y "Black")) '(0 1 3 4 6 7))
    (color-message 2
                   (if (equal? "..." (current-test-value)) "DeepPink"
                       (if (eval temp-test) "RoyalBlue" "DeepPink")))
    (map choose-message-color-at '(5 8))))   
 

;; COLOR THE TEXT FIELDS
(define field-list
  (list test-field pass-field fail-field))
(define value-at-field
  (list '(current-test-value)
        '(current-pass-value)
       '(current-fail-value)))

(define (color-field-at a-position a-color)
  (send (list-ref field-list a-position) set-field-background
        (make-object color% a-color)))

(define (white-all-fields)
  (map (lambda (x) (color-field-at x "white")) '(0 1 2)))

(define (choose-field-color-at a-position)
  (begin
    (white-all-fields)
    (color-field-at a-position
                    (if (equal? "..." (eval (list-ref value-at-field a-position)))
                        "LightCoral" "MediumTurquoise"))))

; the initial state of the illustration pane is blank
(define (f5-illustration-pane)
  (begin
    (send time-slider set-value 0)
    (white-messages-but '())
    (white-all-fields)))

;; THE WORKFLOW CORRESPONDING TO THE SLIDER
(define (step-0)
  (begin
    (color-all-messages)
    (white-messages-but '())
    (white-all-fields)))

(define (step-1)
  (begin
     (color-all-messages)
    (white-messages '(2 5 8))
    (white-all-fields)))

(define (step-2)
  (begin
 (color-all-messages)
    (white-messages '(5 8))
    (choose-field-color-at 0)))

(define (step-3)
    (begin
      (color-all-messages)
      (if (equal? "..." (current-test-value))
          (color-field-at 0 "LightCoral")
          (if (eval temp-test)
              (begin
                (white-messages '(5 6 7 8))
                (choose-field-color-at 1))
              (begin
                (white-messages '(3 4 5 8))
                (choose-field-color-at 2))))))

(define (step-4)
  (begin
 (color-all-messages)
 (if (equal? "..." (current-test-value))
     (color-field-at 0 "LightCoral")
     (if (eval temp-test)
         (begin
           (white-messages '(6 7 8))
           (choose-field-color-at 1))
         (begin
           (white-messages '(3 4 5))
           (choose-field-color-at 2))))))

;; final message
(new message%
     [parent big-frame]
     [label "hi there"])

(send big-frame show #t)
