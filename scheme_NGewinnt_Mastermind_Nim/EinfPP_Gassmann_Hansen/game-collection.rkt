;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname game-collection) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(require "io.rkt" "nim.rkt" "mastermind.rkt" "connect-n.rkt" "nim-plus.rkt"
         "list-procedures.rkt")

; Startet die Spielesammlung und fragt, welches Spiel man spielen will.
(: play-a-game ( -> unspecific))

(define play-a-game
  (lambda ()
    (letrec ((game (begin
                     (write-string
                      (string-append (msg "game?")
                                     (msg "close?")
                                     (msg "game1") "Nim\n"
                                     (msg "game2") "Mastermind\n"
                                     (msg "game3") "Connect-n\n"))
                     (string->number(read-line)))))
      (cond ((not (natural? game)) (begin
                                     (write-string (msg "wrongnumber"))
                                     (play-a-game)))
            ((= game 0) (write-string (msg "close")))
            ((= game 1) (choose-a-nim))
            ((= game 2) (choose-a-mmind))
            ((= game 3) (start-connect-n))
            (else (begin
                    (write-string (msg "wrongnumber2"))
                    (play-a-game)))))))

; Fragt, ob man ein neues Spiel spielen will, wenn ja, dann wird play-a-game
; aufgerufen, wenn nein, dann wird das Spiel beendet.
(: next-game (  -> unspecific))

(define next-game
  (lambda ()
    (letrec ((game (begin
                     (write-string
                      (string-append (msg "newgame")
                                     (msg "no-newgame")))
                     (read-line))))
      (cond ((string=? game "Ja") (play-a-game))
            ((string=? game "Nein") (write-string (msg "close")))
            (else (begin (write-string (msg "YesorNo"))
                         (next-game)))))))
                                   
; Startet das Spiel N-Gewinnt und fragt die Höhe, Breite und Gewinnbedingung ab.
(: start-connect-n ( -> unspecific))

(define start-connect-n
  (lambda ()
    (letrec ((width (begin
                      (write-string (msg "width?"))
                      (string->number(read-line))))
             (height (begin
                       (write-string (msg "height?"))
                       (string->number(read-line))))
             (winning (begin
                        (write-string (msg "winning-length?"))
                        (string->number(read-line)))))
      (if (and (natural>0? width) (natural>0? height) 
               (natural>0? winning) 
               (or (>= width winning)
                   (>= height winning)))
          (begin 
            (connect-n width height winning)
            (next-game))
          (begin
            (write-string (string-append (msg "Natural>0!")
                                         (msg "no-winning!")))
            (start-connect-n))))))

; Fragt welche Nim Version man Spielen will, bzw ob man zurück ins Hauptmenü
; will oder das Spiel beenden will.
(: choose-a-nim ( -> unspecific))

(define choose-a-nim
  (lambda ()
    (letrec ((game (begin
                     (write-string
                      (string-append (msg "game?")
                                     (msg "close?")
                                     (msg "game1") "Standard-Nim.\n"
                                     (msg "game2") "Nim-Triangle.\n"
                                     (msg "game3") "Nim-Rectangle\n"
                                     (msg "back")))
                     (string->number(read-line)))))
      
      (cond ((not (natural? game)) (begin
                                     (write-string (msg "wrongnumber"))
                                     (choose-a-nim))) 
            ((= game 0) (write-string (msg "close")))
            ((= game 1) (begin (start-nim) (next-game)))
            ((= game 2) (begin (start-nim-triangle) (next-game)))
            ((= game 3) (begin (start-nim-rectangle) (next-game)))
            ((= game 4) (play-a-game))
            (else (begin
                    (write-string (msg "wrongnumber2"))
                    (choose-a-nim)))))))

; Startet das Nim-Spiel und fragt ab wie viele Stäbchen in der jeweiligen Reihe
; am Anfang stehen sollen.
(: start-nim ( -> unspecific))

(define start-nim
  (lambda ()
    (letrec ((gamefield (begin
                          (write-string (msg "width"))
                          (list-breaker(split-string " " (read-line))))))
      
      (cond ((or (not (list-all-property? gamefield natural?))
                 (empty? gamefield))
             (begin
               (write-string (msg "natural-List"))
               (start-nim)))
            (else (nim gamefield))))))

; Startet das Nim-triangle-Spiel und fragt die Höhe ab.
(: start-nim-triangle ( -> unspecific))

(define start-nim-triangle
  (lambda ()
    (letrec ((height (begin
                       (write-string (msg "height?"))
                       (string->number(read-line)))))
      
      (cond ((not(natural>0? height))
             (begin
               (write-string (msg "Natural>0!"))
               (start-nim-triangle)))
            (else (nim-triangle height))))))

; Startet das Nim-rectangle-Spiel und fragt die Höhe und Breite ab.
(: start-nim-rectangle ( -> unspecific))

(define start-nim-rectangle
  (lambda ()
    (letrec ((height (begin
                       (write-string (msg "height?"))
                       (string->number(read-line))))
             (width (begin
                       (write-string (msg "width?"))
                       (string->number(read-line)))))
      
      (cond ((or (not(natural>0? height)) (not(natural>0? width)))
             (begin
               (write-string (msg "Natural>0!"))
               (start-nim-rectangle)))
            (else (nim-rectangle height width))))))

; Fragt welche Mastermind Version man Spielen will, bzw ob man zurück ins 
; Hauptmenü will oder das Spiel beenden will.
(: choose-a-mmind ( -> unspecific))

(define choose-a-mmind
  (lambda ()
    (letrec ((game (begin
                     (write-string
                      (string-append (msg "game?")
                                     (msg "close?")
                                     (msg "game1") "mmind-given.\n"
                                     (msg "game2") "mmind-random.\n"
                                     (msg "back")))
                     (string->number(read-line)))))
      
      (cond ((not (natural? game)) (begin
                                     (write-string (msg "wrongnumber"))
                                     (choose-a-mmind))) 
            ((= game 0) (write-string (msg "close")))
            ((= game 1) (begin (start-mmind #t) (next-game)))
            ((= game 2) (begin (start-mmind #f) (next-game)))
            ((= game 4) (play-a-game))
            (else (begin
                    (write-string (msg "wrongnumber2"))
                    (choose-a-mmind)))))))

; Startet das Mastermind Spiel und fragt die Parameter Versuche, Farben,
; Lösungslänge(Spielfeldbreite) und gegebenenfalls die Lösungskombination
(: start-mmind (boolean -> unspecific))

(define start-mmind
  (lambda (game)
    (letrec ((guesses (begin
                        (write-string (msg "tries?"))
                        (string->number(read-line))))
             (colors (begin
                       (write-string (msg "colors?"))
                       (string->number(read-line))))
             (lengths (begin
                        (write-string (msg "combi-length?"))
                        (string->number(read-line)))))
      
      (cond ((not(and (natural? guesses) (natural? colors) (natural? lengths)
                      (> guesses 0) (> colors 0) (> lengths 0)))
             (begin (write-string (msg "Natural>0!"))
                    (start-mmind game)))
            ((eq? game #t) (letrec ((combi (begin 
                                             (write-string
                                              (msg "combi?"))
                                             (list-breaker
                                              (split-string " " (read-line))))))
                             (if (error-check colors lengths combi)
                             (mmind-given guesses colors lengths combi)
                             (begin (write-string
                                     (msg "error!"))
                                    (start-mmind game)))))     
            (else (mmind-random guesses colors lengths))))))
 
; Gibt den String zur gegebenen Message aus.
(: msg (string -> string))

(define msg
  (lambda (x)
    (cond ((eq? x "game?") "Welches Spiel möchten sie spielen?\n")
          ((eq? x "close?") "Geben sie 0 ein um das Spiel zu beenden.\n")
          ((eq? x "game1") "Geben sie 1 ein für das Spiel ")
          ((eq? x "game2") "Geben sie 2 ein für das Spiel ")
          ((eq? x "game3") "Geben sie 3 ein für das Spiel ")
          ((eq? x "back") "Geben sie 4 ein um zurück ins Hauptmenü zu kommen.\n")
          ((eq? x "newgame") "Falls sie noch ein Spiel spielen wollen wählen sie Ja.\n")
          ((eq? x "no-newgame") "Falls sie nicht mehr spielen wollen wählen sie Nein.\n")
          ((eq? x "wrongnumber") "Geben sie bitte eine gültige Spielnummer ein!\n")
          ((eq? x "wrongnumber2") "Wählen sie bitte ein gültiges Spiel!\n")
          ((eq? x "close") "Das Programm wird nun beendet.\n")
          ((eq? x "YesorNo") "Wählen sie bitte zwischen Ja oder Nein.\n")
          ((eq? x "width?") "Wählen sie die Breite des Spielfelds.\n")
          ((eq? x "height?") "Wählen sie die Höhe des Spielfelds.\n")
          ((eq? x "Natural>0!") "Geben sie für die Parameter nur natürliche Zahlen > 0 an!\n")
          ((eq? x "winning-length?")  "Wählen sie die Anzahl der Chips die verbunden werden müssen um zu gewinnen.\n")
          ((eq? x "no-winning!") "Die Gewinnlänge muss kleiner als die Höhe oder die Breite sein, sonst kann keiner Gewinnen.\n")
          ((eq? x "width") "Geben sie eine Zahlenfolge mit Stäbchen für die jeweilige Reihe an.\n")
          ((eq? x "natural-List") "Geben sie bitte nur natürliche Zahlen an getrennt durch ein Leerzeichen.\n")
          ((eq? x "tries?") "Geben sie die Anzahl der Versuche an.\n")
          ((eq? x "colors?") "Geben sie die Anzahl der Farben an.\n")
          ((eq? x "combi-length?") "Geben sie die Länge der Kombination an.\n")
          ((eq? x "combi?") "Geben sie die zu erratene Liste ein!\n")
          ((eq? x "error!")"Geben sie eine Liste ein deren Elemente kleiner als die Farbe ist
und die Länge der Liste ihrer einegegebenen Länge entspricht.\n")
          (else "FEHLER"))))
      



    
  
