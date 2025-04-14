;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname string-procedures) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(provide add-tab-before add-tab-after add-linebreak show-column write-line)

; Erhält einen String und setzt einen Tab vorher an.
(: add-tab-before (string -> string))

(define add-tab-before
  (lambda (string)
    (string-append "\t" string)))

; Erhält einen String und setzt einen Tab nacher an.
(: add-tab-after (string -> string))

(define add-tab-after
  (lambda (string)
    (string-append string "\t")))

; Erhält eine Maximallänge und einen Zähler und schreibt dann Zahlen in einen
; String bis einschließlich zur Maximallänge.
(: show-column ( natural natural -> string))

(define show-column 
  (lambda (height count)
    (cond ((= height count) (string-append (number->string count) "\n\n"))
          ((> height count) (string-append (number->string count) " "
                                           (show-column height (+ count 1)))))))

; Erhält einen String und gibt ihn als Bildschirmausgabe aus. 
; Zusätzlich fügt die Prozedur einen Zeilenumbruch hinten dran.
(: write-line (string -> unspecific))

(define write-line
  (lambda (string)
    (write-string (string-append string "\n"))))

; Erhält einen String und setzt einen Zeilenumbruch hinten dran.
(: add-linebreak (string -> string))

(define add-linebreak
  (lambda (string)
    (string-append string "\n")))