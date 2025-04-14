;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname connect-n) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
;(require "io.rkt" "list-procedures.rkt" "connect-n-adt2.rkt")
(require "io.rkt" "list-procedures.rkt" "connect-n-adt.rkt")
(provide connect-n natural>0? )

; Startet ein Spiel n-gewinnt mit den Parametern Breite Höhe und der Sieganzahl
; und fängt Falscheingaben ab.
(: connect-n (natural natural natural -> string))

(define connect-n
  (lambda (width height winning-requirement)
    (if (and (natural>0? width) (natural>0? height) 
             (natural>0? winning-requirement) 
             (or (>= width winning-requirement)
                 (>= height winning-requirement)))
        (begin
          (gamestart winning-requirement)
          (game-master (make-connect-n-game width height) 1 winning-requirement
                       width height 1))
        (violation 
         (string-append
          "Alle Parameter müssen natürliche Zahlen größer 0 sein.\n"
          "Die Gewinnlänge muss kleiner als die Höhe oder die Breite sein, "
          "sonst kann es keinen Gewinner geben.\n")))))

; Prüft ob eine Zahl natürlich ist und größer 0.
(: natural>0? (any -> boolean))

(check-expect (natural>0? "kartoffel") #f)
(check-expect (natural>0? 2) #t)
(check-expect (natural>0? 0) #f)

(define natural>0?
  (lambda (x)
    (and (natural? x) (> x 0))))

; Die Prozedur gibt vor dem Spielbeginn noch einige Informationen zum Spiel aus.
(: gamestart ( natural -> unspecific))

(define gamestart
  (lambda (winning-requirement)
    (write-string
     (string-append
      "Willkommen zu einer Partie "
      (number->string winning-requirement)
      "-gewinnt.\n"
      "Das Spiel wird mit 2 Spielern gespielt.\n"
      "Abwechselnd wählen die Spieler eine Spalte, "
      "in der sie einen Chip einwerfen wollen.\n"
      "Ziel des Spiels ist es eine Reihe von "
      (number->string winning-requirement)
      " Chips vertikal, horizontal oder diagonal zu konstruieren.\n\n"))))

; Die Prozedur überprüft ob das Spiel bereits vorbei ist und gibt somit
; den entsprechenden String dazu aus.
; Falls das Spiel noch läuft, fordert die Prozedur den derzeitigen Spieler
; dazu auf eine Zeile zu wählen, in die etwas reingeworfen werden soll und gibt
; dann an die Prozedur game weiter.
(: game-master
   (connect-n-game natural natural natural natural natural -> string))

(define game-master
  (lambda (gamefield player winning-requirement width height round)
    (cond ((game-over
            gamefield winning-requirement (player-switch player) round)
           (begin (game-field-creator gamefield) 
                  (write-string
                   (string-append "Spieler " 
                                  (number->string (player-switch player)) 
                                  " du hast gewonnen!\n"))))
          ((connect-n-zero-check gamefield)
           (begin (game-field-creator gamefield)
                  (write-string
                   "Das Spiel ist vorbei. Leider gibt es keinen Gewinner.\n")))
          (else (begin
                  (game-field-creator gamefield)
                  (write-string  
                   (string-append    
                    "Spieler " (number->string player) " du bist jetzt dran \n"
                    "In welche Spalte möchtest du einen Chip einwerfen? \n"))
                  (game gamefield player winning-requirement width height))))))

; Nimmt zuerst die Spalte, in der ein Chip reingeworfen werden soll durch ein
; Eingabekasten entgegen, überprüft ob die Eingabe auch gültig ist für unser
; Spielfeld und gibt bei gültiger Eingabe das nun veränderte Spielfeld zurück an
; die Prozedur game-master
(: game (connect-n-game natural natural natural natural -> string))

(define game
  (lambda (gamefield player winning-requirement width height)
    (letrec ((round (string->number(read-line))))
      (if (and (natural>0? round) (>= width round)
               (full-column? gamefield round))
          (game-master
           (insert-chip-in-game gamefield round player)
           (player-switch player) winning-requirement width height round)
          (begin
            (write-string "Die Spalte existiert nicht oder ist bereits voll \n")
            (game gamefield player winning-requirement width height))))))

; Wechselt die Spieler, wenn der erste Spieler am Zug war ist dannach der zweite
; Spieler dran.
(: player-switch (natural -> natural))

(check-expect (player-switch 1) 2)
(check-expect (player-switch 2) 1)

(define player-switch
  (lambda (player)
    (- 3 player)))

