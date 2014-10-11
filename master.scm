#lang racket

(require racket/gui/base
	pict)

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
  (with-handlers
   ([exn:fail? (lambda (v) "...")])
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

; white, black
; deeppink
; royalblue
; mediumturquoise

;; WHITE OUT
; instead of showing different messages, everything will be shown
; then we hide (white-out)  the unnecessary messages
; it's faster this way
(define (white-message-at a-position a-message-list)
  (colorize-pict-message (list-ref a-message-list a-position)
                         " " "white"))

; change color of the message
(define (color-message-at a-position a-color a-message-list a-label-list)
  (colorize-pict-message
   (list-ref a-message-list a-position)
   (eval (list-ref a-label-list a-position))
   a-color))

(provide current-data
         current-value
         create-pict-message
         colorize-pict-message
	white-message-at
	color-message-at
	)
