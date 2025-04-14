;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname connect-n-adt2) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(require "io.rkt" "list-procedures.rkt" "string-procedures.rkt")

(provide connect-n-game game-field-creator connect-n-zero-check 
         insert-chip-in-game make-connect-n-game game-over full-column?)

; Erstellt ein Tripel, was eine Position im Spiel wiedergibt.
; Connect-n-point ist der Recordname, make-connect-n-point erstellt einen
; Record aus den Selektoren, connect-n-point prüft, ob es dieser Record ist
; und point-width steht für die Spalte in der man sich im Spielfeld befindet,
; point-hight steht für die Zeile in der man im Spielfeld ist und
; point-value sagt welcher Wert an dieser(von Spalte und Zeile) Position steht.
(define-record-procedures connect-n-point
  make-connect-n-point
  connect-n-point?
  (point-width point-height point-value))

(: make-connect-n-point (natural natural natural -> connect-n-point))
(: connect-n-point? (any -> boolean))
(: point-width  ( connect-n-point -> natural))
(: point-height ( connect-n-point -> natural))
(: point-value  ( connect-n-point -> natural))

; Signatur für ein N-Gewinnt-Spielfeld (eine Liste von Records)
(define connect-n-game
  (signature
   (list-of connect-n-point)))

; Konstruktor für ein N-Gewinnt-Spielfeld
; Erhält die Breite und die Höhe des Spielfeldes und erstellt ein Spielfeld
; mit Hilfe von make-connect-n-game-help.
(: make-connect-n-game (natural natural -> connect-n-game))

(define make-connect-n-game
  (lambda (width height)
    (make-connect-n-game-help width height 1 1)))

; Hilfsprozedur für den Konstruktor
; Erhält von make-connect-n-game die Breite, die Höhe, eine 1 für index und eine
; 1 für h-index. Die Prozedur zählt den index (für die Breite) und h-index
; (für die Höhe) hoch. Und erstellt für jede Höhe/Breite Kombination einen
; Record mit dem Wert 0. Wenn der Höhenindex gleich der Höhe ist oder der
; Breitenindex gleich der Breite ist, zählt er den jeweiligen nicht mehr höher.
(: make-connect-n-game-help (natural natural natural natural -> connect-n-game))

(define make-connect-n-game-help
  (lambda (width height index h-index)
    (cond ((and (>= height h-index) (>= width index))
           (cons (make-connect-n-point index h-index 0)
                 (make-connect-n-game-help width height (+ index 1) h-index)))
          ((>= height h-index)
           (make-connect-n-game-help width height 1 (+ h-index 1)))
          (else empty))))

; Fügt einen Chip in das Spielfeld hinzu.
; Die Prozedur erhält eine Liste von Records, die Spalte in der etwas geändert 
; werden soll und welcher Spieler an der Reihe ist (aus der Datei connect-n.rkt)
; Den Chip fügt die Prozedur hinzu, indem sie guckt, wo in der gegebenen Spalte
; der Record-Wert in der Zeile (von unten nach oben) das erste mal = 0 ist und
; wenn er diesen gefunden hat, ändert er ihn auf den Spielerwert.
(: insert-chip-in-game (connect-n-game natural natural -> connect-n-game))
  
(define insert-chip-in-game
  (lambda (gamefield column player)
    (if (and (= (point-width (first gamefield)) column)
             (= (point-value (first gamefield)) 0))
        (cons (make-connect-n-point (point-width(first gamefield))
                                    (point-height (first gamefield))
                                    player) (rest gamefield))
        (cons (first gamefield)
              (insert-chip-in-game (rest gamefield) column player)))))

; Erstellt eine Reihe des Spielfelds als String. Der Index gibt an welche Reihe
; als String erstellt werden soll.
(: gameline-creator (connect-n-game natural -> string))

(define gameline-creator
  (lambda (gamefield index)
    (cond ((empty? gamefield) "\n")
          ((= (point-height (first gamefield)) index)
           (string-append
            (number->string
             (point-value (first gamefield)))
            " "
            (gameline-creator (rest gamefield) index)))
           (else (gameline-creator (rest gamefield) index)))))

;Erstellt das Spielfeld als String
(: gamefield-creator-help (connect-n-game natural -> string))

(define gamefield-creator-help
  (lambda (gamefield count)
    (cond ((= 0 count) "\n")
          (else (string-append
                 (add-tab-after (number->string count))
                 (gameline-creator gamefield count)
                 (gamefield-creator-help gamefield (- count 1)))))))

; Gibt das Spielfeld als Bildschirmausgabe aus.
; Bekommt ein Spielfeld (aus Records) von der Datei connect-n.rkt und schreibt
; die Bildschirmausgabe, indem er die Spalten beschriftet (show-column) und das
; Spielfeld mit Hilfe von gamefield-creator-help als String ausgibt.
(: game-field-creator (connect-n-game -> unspecific))

(define game-field-creator
  (lambda (gamefield)
    (write-string
     (string-append
      (add-tab-before
       (show-column (get-width-or-height gamefield 1 point-width) 1))
      (gamefield-creator-help gamefield
                             (get-width-or-height gamefield 1 point-height))))))

; Erhält ein gamefield und einen count der bei 1 startet und speichert somit die
; Breite bzw. Höhe des Spielfelds in count ab und gibt diese auch aus.
(: get-width-or-height
   (connect-n-game natural (one-of point-width point-height) -> natural))

(check-expect (get-width-or-height  (make-connect-n-game 2 2) 1 point-height) 2)
(check-expect (get-width-or-height  (make-connect-n-game 2 2) 1 point-width) 2)
(check-expect (get-width-or-height  (make-connect-n-game 2 7) 1 point-height) 7)

(define get-width-or-height
  (lambda (gamefield count p)
    (cond ((empty? gamefield) count)
          ((> (p (first gamefield)) count)
           (get-width-or-height (rest gamefield) (+ count 1) p))
          (else (get-width-or-height (rest gamefield) count p)))))

; Prüft, ob alle Felder voll sind, also es unentschieden ist.
; Indem er prüft alle Record-Werte, ob sie = 0 sind. Wenn einer = 0 ist, prüft
; er nicht mehr die anderen, weil er schon ausgibt, dass das Spiel noch nicht
; voll ist.
(: connect-n-zero-check (connect-n-game -> boolean))

(check-expect (connect-n-zero-check (make-connect-n-game 2 2)) #f)
(check-expect 
 (connect-n-zero-check (insert-chip-in-game (make-connect-n-game 2 2)1 1)) #f)
(check-expect 
 (connect-n-zero-check (insert-chip-in-game (make-connect-n-game 1 1)1 1)) #t)

(define connect-n-zero-check
  (lambda (gamefield)
    (cond ((empty? gamefield) #t)
          ((zero? (point-value(first gamefield))) #f)
          (else (connect-n-zero-check (rest gamefield))))))

; Überprüft ob ein Spieler durch eine Reihe in einer simplen Liste
; gewonnen hat.
; Bekommt von der game-over Prozeduren (row, column, giagonal) die
; Gewinnbedingung, den Spieler der dran ist und den Zählerstartwert 0.
; und addiert dann den Zähler um 1, wenn der Spieler der dran ist, 
; an der gegebenen Stelle ist. Wenn der Spieler nicht an der gegeben Stelle
; ist, wird der Zähler auf 0 gesetzt. Außerdem prüft er ob der Zähler = der
; Gewinnbedingung ist. Wenn es so ist, dann hört er auf und der Spieler hat
; gewonnen.
(: game-over-line (connect-n-game natural natural natural -> boolean))

(check-expect 
 (game-over-line(make-row-or-column-from-gamefield(insert-chip-in-game
  (insert-chip-in-game(insert-chip-in-game(insert-chip-in-game 
  (make-connect-n-game 2 2)1 1) 1 2) 2 2) 2 1) 2 point-width) 2 1 0) #f)
(check-expect 
 (game-over-line(make-row-or-column-from-gamefield(insert-chip-in-game
  (insert-chip-in-game(insert-chip-in-game(insert-chip-in-game 
  (make-connect-n-game 2 2)1 1) 1 2) 2 2) 2 1) 2 point-height) 2 2 0) #f)
(check-expect 
 (game-over-line(make-row-or-column-from-gamefield(insert-chip-in-game
  (insert-chip-in-game(insert-chip-in-game(insert-chip-in-game 
  (make-connect-n-game 2 2)1 1) 1 2) 2 2) 2 1) 2 point-width) 1 1 0) #t)

(define game-over-line
  (lambda (gameline winning-requirement player count)
    (cond ((= count winning-requirement) #t)
          ((empty? gameline) #f)
          ((= player (point-value (first gameline)))
           (game-over-line (rest gameline) winning-requirement player 
                           (+ count 1)))
          (else (game-over-line
                 (rest gameline) winning-requirement player 0)))))

; Erhält ein connect-n-game und einen index und erstellt daraus eine Reihe bzw. 
; Spalte des Spielfelds als Liste. Indem er prüft, ob der Index dem Spalten- 
; bzw. Reihen-Wert hat.
(: make-row-or-column-from-gamefield
   (connect-n-game natural (one-of point-width point-height) -> connect-n-game))

(define make-row-or-column-from-gamefield
  (lambda (gamefield index p)
    (cond ((empty? gamefield) empty)
          ((= index (p (first gamefield)))
           (cons (first gamefield)
                 (make-row-or-column-from-gamefield (rest gamefield) index p)))
          (else (make-row-or-column-from-gamefield (rest gamefield) index p)))))

; Erstellt eine Diagonale von einem gegebenen Punkt im Spielfeld aus, die
; entweder von links nach rechts oder von rechts nach links geht.
; Das ist abhängig davon, ob p + oder - erhält.
; Index steht für die Breite und Position für die Höhe.
; Die Liste entsteht, indem er die Diagonale durchgeht und jeden Wert in die 
; Liste schreibt.
(: make-diagonal-from-gamefield
   (connect-n-game natural number (one-of + -) -> connect-n-game))

(define make-diagonal-from-gamefield
  (lambda (gamefield index position p)
    (cond ((empty? gamefield) empty)
          ((and (= index (point-width (first gamefield)))
                (= position (point-height (first gamefield))))
           (cons (first gamefield)
                 (make-diagonal-from-gamefield
                  (rest gamefield) (p index 1) (+ position 1) p)))
          (else (make-diagonal-from-gamefield
                 (rest gamefield) index position p)))))

; Bekommt das Spielfeld und die Position, die zuletzt geändert worden ist.
; Erstellt eine Diagonale von links nach rechts von der Zuletzt veränderten 
; Diagonale.
(: get-left-to-right (connect-n-game natural natural -> connect-n-game))
(define get-left-to-right
  (lambda (gamefield index position)
    (cond ((or (= index 1) (= position 1))
           (make-diagonal-from-gamefield gamefield index position +))
          (else (get-left-to-right gamefield (- index 1) (- position 1))))))

; Bekommt das Spielfeld und die Position, die zuletzt geändert worden ist.
; Erstellt eine Diagonale von rechts nach links von der Zuletzt veränderten 
; Diagonale.
(: get-right-to-left (connect-n-game natural natural -> connect-n-game))
(define get-right-to-left
  (lambda (gamefield index position)
    (cond ((or (= index (get-width-or-height gamefield 1 point-width))
               (= position 1))
           (make-diagonal-from-gamefield gamefield index position -))
          (else (get-right-to-left gamefield (+ index 1) (- position 1)))))) 

; Überprüft ob in der Zeile in der der neue Chip liegt die Siegbedingung erfüllt
; wurde.
(: game-over-row (connect-n-game natural natural natural -> boolean))

(check-expect 
 (game-over-row(insert-chip-in-game(insert-chip-in-game(insert-chip-in-game
 (insert-chip-in-game(make-connect-n-game 2 2)1 1) 1 2) 2 2) 2 1) 2 1 1) #f)
(check-expect(game-over-row(insert-chip-in-game(insert-chip-in-game
  (make-connect-n-game 2 2)1 2) 2 1)2 1 2) #f)
(check-expect(game-over-row(insert-chip-in-game(insert-chip-in-game
  (make-connect-n-game 2 2)1 1) 2 1)2 1 2) #t)

(define game-over-row
  (lambda (gamefield winning player round)
    (game-over-line
     (make-row-or-column-from-gamefield
      gamefield (get-row gamefield round 0) point-height)
     winning player 0)))

; Überprüft, ob in der Spalte in der der neue Chip liegt die Siegbedingung
; erfüllt wurde.
(: game-over-column (connect-n-game natural natural natural -> boolean))

(check-expect 
 (game-over-column(insert-chip-in-game(insert-chip-in-game(insert-chip-in-game
 (insert-chip-in-game(make-connect-n-game 2 2)1 1) 1 2) 2 2) 2 1) 2 1 1) #f)
(check-expect(game-over-column(insert-chip-in-game(insert-chip-in-game
  (make-connect-n-game 2 2)1 2) 1 1)2 1 1) #f)
(check-expect(game-over-column(insert-chip-in-game(insert-chip-in-game
  (make-connect-n-game 2 2)1 1) 1 1)2 1 1) #t)

(define game-over-column
  (lambda (gamefield winning player round)
    (game-over-line
     (make-row-or-column-from-gamefield gamefield round point-width)
     winning player 0)))

; Überprüft ob in den Diagonalen in der der neue Chip liegt die Siegbedingung
; erfüllt wurde.
(: game-over-diagonal (connect-n-game natural natural natural -> boolean))

(check-expect 
 (game-over-diagonal(insert-chip-in-game(insert-chip-in-game(insert-chip-in-game
 (insert-chip-in-game(make-connect-n-game 2 2)1 1) 1 2) 2 2) 2 1) 2 1 2) #t)
(check-expect(game-over-diagonal(insert-chip-in-game(insert-chip-in-game
  (make-connect-n-game 2 2)1 2) 1 1)2 1 1) #f)
(check-expect(game-over-diagonal(insert-chip-in-game(insert-chip-in-game
  (make-connect-n-game 2 2)1 1) 1 1)2 1 1) #f)

(define game-over-diagonal
  (lambda (gamefield winning player round)
    (or (game-over-line
         (get-left-to-right
          gamefield round (get-row gamefield round 0))
         winning player 0)
        (game-over-line
         (get-right-to-left
          gamefield round (get-row gamefield round 0))
         winning player 0))))

; Gibt aus in welcher Zeile der eingeworfene Chip nun liegt.
; Bekommt das Spielfeld, die Spalte in die eingeworfen wurde und den Zählerstart
; 0. Es wird geprüft, ob der Wert in der Zeile ungleich 0 ist, wenn ja dann
; dann zählt er den Zähler um 1 hoch und wenn der Wert Null ist gibt er den
; Zähler aus, also die Zeile in der zu letzt etwas geändert wurde.
(: get-row (connect-n-game natural natural -> natural))

(check-expect (get-row(insert-chip-in-game(insert-chip-in-game
              (make-connect-n-game 2 2)1 1) 1 2) 1 0) 2)
(check-expect (get-row(insert-chip-in-game(insert-chip-in-game
              (make-connect-n-game 2 2)1 1) 2 2) 2 0) 1)
(check-expect (get-row(insert-chip-in-game(make-connect-n-game 2 2)1 1) 1 0) 1)

(define get-row
  (lambda (gamefield round count)
    (cond ((empty? gamefield) count)
          ((and (= round (point-width (first gamefield)))
                 (not(zero? (point-value (first gamefield)))))
           (get-row (rest gamefield) round (+ count 1)))
          ((and (= round (point-width (first gamefield)))
                (zero? (point-value (first gamefield))))
           count)
          (else 
           (get-row (rest gamefield) round count)))))

; Überprüft, ob der derzeitige Spieler das Spiel mit seinem letzten Zug
; gewonnen hat.
(: game-over (connect-n-game natural natural natural -> boolean))

(check-expect(game-over(insert-chip-in-game(insert-chip-in-game
 (insert-chip-in-game(make-connect-n-game 2 2)1 1) 2 2) 2 1) 2 1 2) #t)
(check-expect(game-over(insert-chip-in-game(insert-chip-in-game
 (make-connect-n-game 2 2)1 1) 1 2) 2 2 1) #f)
(check-expect (game-over(insert-chip-in-game(insert-chip-in-game
 (insert-chip-in-game(make-connect-n-game 2 2)1 1) 1 2) 2 1) 2 1 2) #t)

(define game-over
  (lambda (gamefield winning player round)
    (or
     (game-over-row gamefield winning player round)
     (game-over-column gamefield winning player round)
     (game-over-diagonal gamefield winning player round))))

; Überprüft, ob in der Spalte in die eingeworfen wird noch Platz für einen
; weiteren Chip gibt, ist noch Platz vorhanden wird true ausgegeben und 
; falls nicht wird false ausgegeben.
(: full-column? (connect-n-game natural -> boolean))

(check-expect(full-column?(insert-chip-in-game(insert-chip-in-game
 (insert-chip-in-game(make-connect-n-game 2 2)1 1) 2 2) 2 1) 2) #f)
(check-expect(full-column?(insert-chip-in-game(insert-chip-in-game
 (insert-chip-in-game(make-connect-n-game 2 2)1 1) 2 2) 2 1) 1) #t)
(check-expect(full-column?(make-connect-n-game 2 2) 1) #t)

(define full-column?
  (lambda (gamefield index)
    (full-column?-help
     (make-row-or-column-from-gamefield gamefield index point-width))))

; Hilfsprozedur um festzustellen, ob noch Platz in der Spalte vorhanden ist.
; Indem er prüft, ob in der Spalte ein Record-Wert den Wert 0 hat. Wenn ein Wert
; noch 0 ist, dann ist die Spalte noch nicht voll.
(: full-column?-help (connect-n-game -> boolean))

(check-expect(full-column?-help(make-row-or-column-from-gamefield
 (insert-chip-in-game(insert-chip-in-game(insert-chip-in-game
 (make-connect-n-game 2 2)1 1) 2 2) 2 1) 2 point-width)) #f)
(check-expect(full-column?-help(make-row-or-column-from-gamefield
 (insert-chip-in-game(insert-chip-in-game(insert-chip-in-game
 (make-connect-n-game 2 2)1 1) 2 2) 2 1) 1 point-width)) #t)
(check-expect(full-column?-help(make-row-or-column-from-gamefield
 (make-connect-n-game 2 2) 1 point-width)) #t)

(define full-column?-help
  (lambda (gamefield)
    (cond ((empty? gamefield) #f)
          ((= (point-value(first gamefield)) 0) #t)
          (else (full-column?-help (rest gamefield))))))          