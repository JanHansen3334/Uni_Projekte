(module io scheme
  
  (require racket/serialize)
  (require (only-in deinprogramm/DMdA-advanced (write-string write-string-dmda)))
  (require (only-in racket (string-split string-split-racket)))
  
  (provide read-line write-string split-string)
  
  (define write-string
    (lambda (str)
      (begin (write-string-dmda str) "finished")))
  
  (define whitespace-char?
    (lambda (char)
      (or (string=? char " ") (or (string=? char "\n")))))
  
  (define split-string
    (lambda (delimitter string)
      (string-split-racket string delimitter)))
  )

