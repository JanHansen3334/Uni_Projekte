;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname list-procedures) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(require "io.rkt")

(provide elem? delete-elem list-breaker list-all-property? delete-one-of-elem
         list-changer property-check? replicate)


; Überprüft ob ein Element in einer Liste vorhanden ist.
(: elem? (any (list-of any) -> boolean))

(check-expect (elem? 4 (list 1 2 3)) #f)
(check-expect (elem? "a" (list 7 "a" 3)) #t)
(check-expect (elem? 0 empty) #f)

(define elem?
  (lambda (x s)
    (cond ((empty? s) #f)
          ((cons? s) (or (eq? x (first s))
                         (elem? x (rest s)))))))


; Löscht ein bestimmtes Element aus einer Liste
(: delete-elem (%a (list-of %b) -> (list-of %b)))

(check-expect (delete-elem 5 (list 1 2 5)) (list 1 2))
(check-expect (delete-elem -3 (list -3 5 4 6)) (list 5 4 6))
(check-expect (delete-elem 0 (list 1 0 2 0 5 0)) (list 1 2 5))
(check-expect (delete-elem "a" (list 1 0 "a" 0 "a" 0)) (list 1 0 0 0))

(define delete-elem
  (lambda (x xs)
    (cond ((empty? xs) empty)
          ((eq? x (first xs)) (delete-elem x (rest xs)))
          (else (cons (first xs) (delete-elem x (rest xs)))))))

; Löscht ein bestimmtes Element an der ersten Stelle, wo es in der Liste
; vorkommt.
(: delete-one-of-elem (%a (list-of %b) -> (list-of %b)))

(check-expect (delete-one-of-elem 5 (list 1 2 5)) (list 1 2))
(check-expect (delete-one-of-elem "a" (list -3 5 "a" 6)) (list -3 5 6))
(check-expect (delete-one-of-elem 0 (list 1 0 2 0 5 0)) (list 1 2 0 5 0))

(define delete-one-of-elem
  (lambda (x xs)
    (cond ((empty? xs) empty)
          ((eq? x (first xs)) (rest xs))
          (else (cons (first xs) (delete-one-of-elem x (rest xs)))))))

; Erstellt aus einer Liste von Strings eine Liste aus Zahlen
; Falsche Eingaben werden vorher abgefangen es kann nur eine string-Liste sein
; mit Stringzahlen.
(: list-breaker ((list-of string) -> (list-of real)))

(check-expect (list-breaker (list "1" "2" "3")) (list 1 2 3))
(check-expect (list-breaker empty) empty)
(check-expect (list-breaker (list "31" "2" "7")) (list 31 2 7))

(define list-breaker
  (lambda (xs)
    (if (empty? xs)
        empty
        (cons (string->number (first xs)) (list-breaker (rest xs))))))

; Erhält einen Zahlenwert und eine Operationd der Art <, >, <=, >=, = 
; und gibt eine Prozedur weiter die eine Variable x und max mit der 
; Operation verarbeitet.
(: property-check? ( %a (%a %b -> boolean) -> ( %a -> boolean)))
   
(check-expect ((property-check? 4 <) 3) #t)
(check-expect ((property-check? 4 =) 3) #f)
(check-expect ((property-check? 4 >) 32) #t)

(define property-check?
  (lambda (max p?)
    (lambda (x)
      (p? x max))))
    
; Erhält als Eingabe eine Liste und ein Prädikat und überprüft,
; ob jedes Element der Liste das Prädikat erfüllt.
(: list-all-property? ( (list-of %a) (any -> boolean) -> boolean))

(check-expect (list-all-property? (list 0 0 0) zero?) #t)
(check-expect (list-all-property? (list 1 2 -4) natural?) #f)
(check-expect (list-all-property? (list 0 0 0) boolean?) #f)
 
(define list-all-property?
  (lambda (game-field p?)
    (cond ((empty? game-field) #t)
          ((p? (first game-field)) (list-all-property? (rest game-field) p?))
           (else #f))))

; Nimmt eine Liste und zieht aus einer gewählten Listenposition (Zeile)
; den gewählten Wert sticks (Anzahl entnommener Stäbchen) ab.
; game-field ist dabei das Spielfeld in Form einer Liste aus natürlichen Zahlen
; line ist die Zeile oder Listenposition aus der Stäbchen entfernt werden sollen
; sticks ist die Anzahl der Stäbchen, die an der gewünschten
; Listenposition entfernt werden sollen.
; Hier wird eine sinnvolle Eingabe erwartet, da ungültige Eingaben in vorherigen
; Prozeduren abgefangen werden.
(: list-changer ((list-of natural) natural natural -> (list-of natural)))

(check-expect (list-changer (list 3 3 3) 2 3) (list 3 0 3))
(check-expect (list-changer (list 1 0 0) 1 1) (list 0 0 0))
(check-expect (list-changer (list 23 40 39) 3 20) (list 23 40 19))

(define list-changer
  (lambda (game-field line sticks)
    (if (= line 1)
        (cons (- (first game-field) sticks) (rest game-field))
        (cons (first game-field)
              (list-changer (rest game-field) (- line 1) sticks)))))

; Erstellt eine Liste aus der gegebenen Länge aus dem gegebenen Symbol.
; Der Parameter length bestimmt die Länge der Liste.
; Der Parameter symbol bestimmt die Elemente der Liste.
(: replicate (natural %a -> (list-of %a)))

(check-expect (replicate 3 4) (list 4 4 4))
(check-expect (replicate 5 "a") (list "a" "a" "a" "a" "a"))
(check-expect (replicate 2 1) (list 1 1))

(define replicate
  (lambda (length symbol)
    (if (= 0 length)
        empty
        (cons symbol (replicate (- length 1) symbol)))))