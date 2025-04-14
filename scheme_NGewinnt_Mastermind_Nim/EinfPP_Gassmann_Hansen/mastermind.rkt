;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname mastermind) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(require "io.rkt" "list-procedures.rkt" "string-procedures.rkt")
(provide mmind-random mmind-given input-check error-check)

; Erstellt eine zufällige Kombination als Liste von natürlichen Zahlen.
; Erhält als Eingabe die Länge der Kombination und die Anzahl der Farben.
; Die natürlichen Zahlen, die Zahl 0 miteingeschlossen stellen die
; Farben des Mastermindspiels dar.
(: combi-creator (natural natural -> (list-of natural)))

(define combi-creator
  (lambda (length colors)
    (if (= length 0)
        empty
        (cons (random colors) (combi-creator (- length 1) colors)))))

; Startet ein Mastermind-Spiel mit einer zufälligen generierten 
; Farbenkombination als Lösung.
; Erste Eingabe bildet die Anzahl der Versuche
; Zweite Eingabe bildet die Anzahl der "Farben" in Form von natürlichen
; Zahlen
; Dritte Eingabe ist die Länge der zu erratenen Farbenkombination
; Die Prozedur gibt die Eingaben zusammen mit einer zufällig generierten Liste
; an die Hauptprozedur weiter.
(: mmind-random ( natural natural natural -> unspecific))

(define mmind-random
  (lambda (guesses colors length)
    (if (input-check guesses colors length)
        (mmind guesses colors length (combi-creator length colors))
        (violation "Die Eingaben müssen natürliche Zahlen >0 sein!"))))

; Die möglichen Falscheingaben die in mmind-random und mmind-given gleich sind
; werden abgefangen. Dabei muss es sich bei allen Eingaben um natürliche Zahlen
; handeln, die außerdem größer als 0 sind.
(: input-check (%a %a %a -> boolean))

(check-expect (input-check 5 3 3) #t)
(check-expect (input-check 0 0 0) #f)
(check-expect (input-check 5 "a" 3) #f)

(define input-check
  (lambda (guesses colors length)
    (and (natural? guesses) (natural? colors) (natural? length)
         (> guesses 0) (> colors 0) (> length 0))))

; Startet ein Mastermind-Spiel mit einer gegebenen Farbenkombination als Lösung.
; Erste Eingabe bildet die Anzahl der Versuche.
; Zweite Eingabe bildet die Anzahl der "Farben" in Form von natürlichen Zahlen.
; Dritte Eingabe ist die Länge der zu erratenen Farbenkombination.
; Vierte Eingabe ist die zu erratene Farbenkombination in Form einer Liste aus 
; natürlichen Zahlen.
; Die Prozedur gibt die Eingaben zusammen mit einer zufällig generierten Liste
; an die Hauptprozedur weiter.
(: mmind-given ( natural natural natural (list-of natural) -> unspecific))

(define mmind-given
  (lambda (guesses colors lengths combi)
    (if (and (input-check guesses colors lengths)
             (error-check colors lengths combi))
        (mmind guesses colors lengths combi)
        (violation
         (string-append
          "Die eingegebenen Zahlen müssen natürliche Zahlen sein (und > 0). "
          "Außerdem muss die Liste aus natürlichen Zahlen bestehen.")))))

; Hauptprozedur
; Erhält ihre Parameter entweder von mmind-given oder mmind-random
; und startet sozusagen das Spiel, indem es die Parameter an die 
; game-master Prozedur weitergibt.
(: mmind (natural natural natural (list-of natural) -> unspecific ))

(define mmind
  (lambda (guesses colors length combi)
    (game-master guesses colors length combi 0 0)))

; Prüft, ob das Spiel zuende ist und gibt aus wie viele Versuche bereits
; gemacht worden sind. Hier erwarten wir sinnige Eingaben, da wir sie
; vorher schon abfangen.
(: game-master 
   (natural natural natural (list-of natural) natural natural -> unspecific))

(define game-master
  (lambda (guesses colors lengths combi blacks current-tries)
    (cond ((= blacks (length combi)) (write-line "Hurra! Du hast gewonnen!"))
          ((= guesses current-tries) (write-line "Buuhuu, du hast verloren!"))
          (else (begin
                  (write-line
                   (string-append
                    "Verbleibende Versuche: "
                    (add-linebreak (number->string (- guesses current-tries)))
                    "Versuche die Kombination zu erraten"))
                  (game guesses colors lengths combi current-tries))))))

; Fordert die Eingabe für den nächsten Schritt und überprüft diese auf
; unsinnige Eingaben.
(: game (natural natural natural (list-of natural) natural -> unspecific))

(define game
  (lambda (guesses colors length combi current-tries)
    (letrec ((try (begin
                    (write-line "Mache deine Eingabe")
                    (split-string " " (read-line)))))
      (if (error-check colors length (list-breaker try))
          (begin
            (write-line (string-append "---" (strings-list->string try) "---"))
            (game-round (list-breaker try) combi)
            (game-master
             guesses
             colors
             length
             combi
             (detect-blacks (list-breaker try) combi 0)
             (+ current-tries 1)))
          (begin
            (write-line "Fehlerhafte Eingabe : Mache deine Eingabe erneut")
            (game guesses colors length combi current-tries))))))

; Prozedur um festzustellen ob die eingegebene Liste den Regeln des Spiels 
; entspricht.
; Die eingegebene Liste muss aus natürlichen Zahlen bestehen und die selbe
; Länge wie die zu erratene Farbkombination besitzen.
(: error-check ( natural natural (list-of %a) -> boolean))

(check-expect (error-check 4 3 (list 1 2 3)) #t)
(check-expect (error-check 4 3 (list 1 2 4)) #f)
(check-expect (error-check 4 3 (list 1 2 3 4)) #f)

(define error-check
  (lambda (colors lengths list)
    (and
     (list-all-property? list natural?)
     (= lengths (length list))
     (list-all-property? list (property-check? (- colors 1) <=)))))

; Erhält den Versuch (als Liste) und zählt die richtigen Treffer an der
; richtigen Position.
; Der Parameter guesslist ist der Versuch, den der Benutzer macht die Farb-
; kombination zu erraten.
; combination ist die zu erratende Farbkombination
; black ist der Zähler, der die richtigen Treffer zählt.
(: detect-blacks ((list-of natural) (list-of natural) natural -> natural))

(check-expect (detect-blacks (list 1 2 3) (list 2 2 4) 0) 1) 
(check-expect (detect-blacks (list 1 2 3 4 5) (list 2 3 3 2 5) 0) 2)
(check-expect (detect-blacks (list 5 5 5 1) (list 1 2 3 5) 0) 0)

(define detect-blacks
  (lambda (guesslist combination black)
    (cond ((empty? guesslist)    black)       
          ((empty? combination)
           (detect-blacks (rest guesslist) combination black))
          ((= (first guesslist) (first combination))
           (detect-blacks  (rest guesslist) (rest combination) (+ black 1)))
          (else (detect-blacks (rest guesslist) (rest combination) black)))))

; Erhält 2 Listen und entfernt die richtigen Treffer aus der ersten Liste.
; Hier erwarten wir eine richtige Eingabe da diese vorher abgefangen werden.
(: white-list ( (list-of natural) (list-of natural) -> (list-of natural)))

(check-expect (white-list (list 1 2 3) (list 2 3 4)) (list 1 2 3))
(check-expect (white-list (list 1 2 3) (list 1 2 3)) empty)
(check-expect (white-list (list 1 2 3) (list 2 2 4)) (list 1 3))


(define white-list
  (lambda (guesslist combination)
    (cond ((empty? guesslist) empty)
          ((= (first guesslist) (first combination))
           (white-list (rest guesslist) (rest combination)))
          (else (cons (first guesslist)
                      (white-list (rest guesslist) (rest combination)))))))

; Erhält 2 Listen und gibt an wie viele Farben richtig geraten wurden,
; allerdings noch nicht an der richtigen Stelle stehen.
; Hier erwarten wir eine richtige Eingabe da diese vorher abgefangen werden.
(: detect-whites ((list-of natural) (list-of natural) natural -> natural))

(check-expect (detect-whites (list 1 2 3) (list 2 3 4) 0) 2) 
(check-expect (detect-whites (list 1 2 3 4 5) (list 2 3 4 2 2) 0) 3)
(check-expect (detect-whites (list 5 5 5 5) (list 1 2 3 4) 0) 0)

(define detect-whites
  (lambda (guesslist combination white)
    (cond ((empty? guesslist) white)
          ((empty? combination) white)
          ((elem? (first guesslist) combination)
           (detect-whites
            (rest guesslist)
            (delete-one-of-elem (first guesslist) combination) (+ white 1)))
          (else (detect-whites (rest guesslist) combination white)))))

; Erhält den Versuch die richtige Kombination zu erraten und die Farbkombination
; und prüft wieviele richtige Treffer und richtige Farben, an falschen
; Positionen vorliegen.
; Um die richtigen Treffer festzustellen wird die Versuchsliste an detect-blacks
; übergeben.
; Um die richtigen Farben an den falschen Positionen festzustellen werden zuerst
; die richtigen Treffer aus der Versuchsliste entfertn durch die Prozedur 
; white-list und diese wird dann an detect-whites weitergegeben.
(: game-round ((list-of natural) (list-of natural) -> unspecific))

(define game-round
  (lambda (guesslist combination)
    (write-line
     (string-append "Schwarze: "
                    (number->string (detect-blacks guesslist combination 0))
                    " Weiße: "
                    (number->string
                     (detect-whites (white-list guesslist combination)
                                    (white-list combination guesslist) 0))))))