;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname nim-plus) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(require "nim.rkt")

(provide nim-rectangle nim-triangle)

; Die Prozedur startet eine Spielpartie nim mit einem dreieckigen Spielfeld
; Die Eingabe lines legt fest aus wie viele Zeilen das dreieckige Spielfeld
; bestehen soll.
; Bei der Eingabe muss es sich um eine natürliche Zahl handeln,
; sonst wird eine Fehlermeldung ausgegeben.
(: nim-triangle ( natural -> string))

(define nim-triangle
  (lambda (lines)
    (if (and (natural? lines) (> lines 0))
        (nim (make-field-with-procedure lines 1 (lambda (x) (+ x 1))))
        (violation
         "Geben sie bitte eine natürliche Zahl ein, die größer als 0 ist, um das Spiel zu starten."))))

; Startet ein Nim-Spiel mit einem rechteckigen Spielfeld, der erste Parameter
; gibt die Zeilenanzahl an und der zweite Parameter gibt die Anzahl der Stäbchen
; pro Zeile an.
; Bei beiden Eingaben muss es sich um natürliche Zahlen handeln,
; sonst wird eine Fehlermeldung ausgegeben.
(: nim-rectangle (natural natural -> string))

(define nim-rectangle
  (lambda (lines sticks)
    (if (and (natural? sticks) (natural? lines) (> lines 0) (> sticks 0))
        (nim (make-field-with-procedure lines sticks (lambda (x) x)))
        (violation
         "Geben sie bitte zwei natürliche Zahlen ein, die größer als 0 sind, um das Spiel zu starten."))))

; Hilfsprozedur für die Erstellung von einem rechteckigem oder dreieckigem
; Spielfeld.
; Erhält als Parameter die gewünschte Anzahl an Zeilen im Parameter n
; und die gewünschte Anzahl der Stäbchen im Parameter sticks.
; Für den Parameter fun erhält die Prozedur eine Prozedur,
; die die Anzahl der Sticks pro Zeile variieren lassen kann.
; Wir erwarten auch hier eine sinnvolle Eingabe.
(: make-field-with-procedure
   ( natural natural (natural -> natural) -> (list-of natural)))

(check-expect (make-field-with-procedure 3 4 (lambda (x) x)) (list 4 4 4))
(check-expect (make-field-with-procedure 3 1 (lambda (x) (+ x 1))) (list 1 2 3))
(check-expect (make-field-with-procedure 0 2 (lambda (x) x)) empty)

(define make-field-with-procedure
  (lambda (n sticks fun)
    (if (<= n 0)
        empty
        (cons sticks (make-field-with-procedure (- n 1) (fun sticks) fun)))))