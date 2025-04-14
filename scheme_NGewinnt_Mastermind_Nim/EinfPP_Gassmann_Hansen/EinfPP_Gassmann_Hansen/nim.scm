;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-assignments-reader.ss" "deinprogramm")((modname nim) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
; Das Spiel Nim : Ein Spiel für zwei Personen, bei dem abwechselnd eine Anzahl von Gegenständen z.B. Streichhölzer,
; gewonnen hat der, der das letzte 'Streichholz' nimmt.
; Exportiert die Prozedur nim
;(provide nim)
(require "io.rkt")

;Die Prozedur nim nimmt eine Liste von natürlichen Zahlen an und ....
;(: nim (list-of natural) -> 

        
(define field
  (lambda (xs)
    (if (empty? xs)
        (violation "Liste ist leer")
        (write-string