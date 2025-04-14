;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname nim) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
; Das Spiel nim ist ein Spiel für zwei Personen, bei dem abwechselnd eine Anzahl
; Stäbchen gezogen werden.
; Gewonnen hat der, der das letzte Stäbchen nimmt.

; Exportiert die Prozeduren nim.
(provide nim)

; Importiert die Dateien io.rkt und list-procedures.rkt
(require "io.rkt" "list-procedures.rkt" "string-procedures.rkt")


; Die Prozedur nimmt eine Liste von natürlichen Zahlen entgegen und beginnt
; damit ein Spiel.
; Außerdem prüft die Prozedur ob die Liste leer ist, falls sie leer ist wird
; eine Fehlermeldung ausgegeben.
; Und prüft ob die Eingabe auch eine Liste von natürlichen Zahlen ist.
(: nim ((list-of natural) -> string))

(define nim
  (lambda (game-field)
    (if (or (empty? game-field) (not(list-all-property? game-field natural?)))
        (violation "Ungültige Eingabe")
        (game-master game-field #t))))

; Erstellt aus der gegebenen Liste einen String der später zum Spielfeld
; verarbeitet wird.
; Die gegebene Liste muss aus natürlichen Zahlen bestehen, ob die Liste leer
; ist wird in der Prozedur nim geprüft, daher darf die Prozedur field->string
; keine leere Liste als Eingabe erhalten.
(: field->string ((list-of natural) natural -> string))

(check-expect (field->string (list 1) 1) "1\t| \n")
(check-expect (field->string (list 3 3) 1) "1\t| | | \n2\t| | | \n")
(check-expect (field->string (list 1 2 3) 1) "1\t| \n2\t| | \n3\t| | | \n")

(define field->string
  (lambda (xs count)
    (letrec (( line-generator 
               ( lambda (x) 
                  (if (> x 0)
                      (begin
                        (string-append
                         "| " 
                         (line-generator (- x 1))))
                      (string-append "\n")))))
      (if (empty? (rest xs))
          (string-append
           (add-tab-after(number->string count))
           (line-generator (first xs)))
          (string-append
           (add-tab-after(number->string count))
           (line-generator (first xs))
           (field->string (rest xs) (+ count 1)))))))

; Die Prozedur bestimmt welcher Spieler an der Reihe ist, bzw. welcher Spieler
; gewonnen hat. Stellt außerdem den Start des Spiels dar ( Anzeige des Feldes ).
; Als Eingabe erhält die Prozedur ein Spielfeld noch in Form einer Liste von
; natürlichen Zahlen und einen Wert der festlegt, welcher Spieler an der Reihe
; ist ( wenn player den Wert 1 hat ist Spieler 1 an der Reihe 
; und wenn player den Wert 0 hat ist Spieler 2 dran.)
(: game-master ((list-of natural) boolean -> any))

(define game-master
  (lambda (game-field player)
    (cond ((list-all-property? game-field zero?)
           (write-string
            (string-append "Spieler " 
                           (player->string (not player))
                           " du hast gewonnen!\n")))
          (else (begin
                  (write-string
                   (string-append
                    "*******************************\n"
                    (field->string game-field 1)
                    "\nSpieler "
                    (player->string player)
                    " du bist dran!\n"))
                  (game-master (game game-field) (not player)))))))

; Die Prozedur fordert die Eingabe der Zeile, aus der die Stäbchen entnommen
; werden sollen und die Anzahl der zu entnehmenden Stäbchen und prüft,ob die
; Eingabe gültig ist und fordert bei falscher Eingabe zur erneuten Eingabe auf.
(: game ((list-of natural) -> any))

(define game
  (lambda (game-field)
    (letrec ((lines
              (begin
                (write-string
                 "Aus welcher Zeile möchtest du Stäbchen entnehmen ?\n")
                (string->number(read-line))))
             (sticks
              (begin 
                (write-string "Wie viele Stäbchen möchtest du entfernen?\n")
                (string->number(read-line)))))
      (if (eq? (error-check game-field lines sticks) #f)
          (list-changer game-field lines sticks)
          (begin
            (write-line (error-check game-field lines sticks))
            (write-line (field->string game-field 1))
            (game game-field))))))

; Nimmt einen Wahrheitswert und ordnet ihm einen Spieler zu.
; In diesem Fall ist true Spieler 1 und false Spieler 2.
(: player->string (boolean -> string))

(check-expect (player->string #f) "2")
(check-expect (player->string #t) "1")

(define player->string
  (lambda (player)
    (if (eq? player #t)
        "1"
        "2")))

; Prüft ob die Eingaben unsinnig sind bzw. nicht erlaubt sind.
; Die beiden Eingaben müssen sowohl natürliche Zahlen sein
; Die ausgewählte Zeile darf nicht kleiner als 1 sein und nicht größer
; als die Länge des Spielfelds
; Die Anzahl der Stäbchen zum Entnehmen darf nicht größer sein als
; die Anzahl der vorhandenen Stäbchen und man muss mindestens ein 
; Stäbchen entnehmen.
(: error-check ((list-of natural) any any -> (mixed boolean string)))

(check-expect (error-check (list 2 2 2) 2 1) #f)
(check-expect (error-check (list 2 3 4) 5 2) 
              "Zeile nicht vorhanden: Mache eine neue Eingabe")
(check-expect (error-check (list 2 2 2) 2 5)
              "Zahl zu groß: Mache eine neue Eingabe")
(check-expect (error-check (list 2 2 2) 2 0)
              "Du musst mindestens ein entnehmen: Mache eine neue Eingabe")
(check-expect
 (error-check (list 2 2 2) "kartoffel" 3)
 "Falsche Eingabe: Geben sie in beiden Fällen natürliche Zahlen ein!")

(define error-check
  (lambda (game-field lines sticks)
    (cond ((not (and (natural? lines) (natural? sticks)))
           "Falsche Eingabe: Geben sie in beiden Fällen natürliche Zahlen ein!")
          ((or (< lines 1) (> lines (length game-field)))
            "Zeile nicht vorhanden: Mache eine neue Eingabe")
          ((> sticks (list-ref game-field (- lines 1)))   
           "Zahl zu groß: Mache eine neue Eingabe")
          ((> 1 sticks)
            "Du musst mindestens ein entnehmen: Mache eine neue Eingabe")
          (else #f))))


                      