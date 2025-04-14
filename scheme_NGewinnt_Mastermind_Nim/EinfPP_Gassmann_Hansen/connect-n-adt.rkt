;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname connect-n-adt) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(require "list-procedures.rkt" "string-procedures.rkt")
(provide connect-n-game make-connect-n-game game-field-creator
         game-field game-line insert-chip-in-game insert-chip-in-column
         full-column? game-over connect-n-zero-check)

; Signatur für N-Gewinnt-Spielfeld.
(define connect-n-game
  (signature
   (list-of (list-of natural))))

; Konstruktor, der eine Liste von Listen durch gegebene Breite und Höhe
; erstellt. Die Breite bestimmt die Anzahl der Listen in der Liste und 
; die Höhe bestimmt die Anzahl der Elemente in den Listen der Liste.
; Die Elemente sind vorerst nur Nullen (0), da es am Anfang ein leeres
; Spielfeld wiedergibt.
(: make-connect-n-game (natural natural -> connect-n-game))

(check-expect (make-connect-n-game 2 2) (list (list 0 0) (list 0 0)))
(check-expect (make-connect-n-game 0 2) empty)
(check-expect (make-connect-n-game 1 3) (list (list 0 0 0)))

(define make-connect-n-game
  (lambda (width height)
    (replicate width (replicate height 0))))

; Erhält eine Liste und den derzeitigen Spieler der N-gewinnt-Runde
; und tauscht die erste 0 in der Liste durch den Wert des Spielers aus.
(: insert-chip-in-column ((list-of natural) natural -> (list-of natural)))

(check-expect (insert-chip-in-column (list 0 0 0) 1) (list 1 0 0))
(check-expect (insert-chip-in-column (list 1 2 0) 1) (list 1 2 1))
(check-expect (insert-chip-in-column (list 2 0 0) 2) (list 2 2 0))

(define insert-chip-in-column
  (lambda (gamefield player)
    (if (= (first gamefield) 0)
        (cons player (rest gamefield))
        (cons (first gamefield) 
              (insert-chip-in-column (rest gamefield) player)))))

; Erhält ein N-gewinnt-Spielfeld und wirft einen Chip hinein.
(: insert-chip-in-game (connect-n-game natural natural -> connect-n-game))

(check-expect (insert-chip-in-game (make-connect-n-game 1 3) 1 1)
              (list (list 1 0 0)))
(check-expect (insert-chip-in-game (make-connect-n-game 2 3) 1 2)
              (list (list 2 0 0) (list 0 0 0)))
(check-expect (insert-chip-in-game (make-connect-n-game 2 2) 2 1)
              (list (list 0 0) (list 1 0)))

(define insert-chip-in-game
  (lambda (gamefield column player)
    (if (= column 1)
        (cons  (insert-chip-in-column (first gamefield) player)
               (rest gamefield))
        (cons (first gamefield) 
              (insert-chip-in-game (rest gamefield) (- column 1) player)))))

; Gibt das Spielfeld (als String) als Bildschirmausgabe aus.
(: game-field-creator (connect-n-game -> unspecific))

(define game-field-creator
  (lambda (gamefield)
    (write-string 
     (string-append (add-tab-before 
                     (show-column (length gamefield) 1))
                    (game-field gamefield 0 1)))))

; Nimmt ein Spielfeld und erstellt daraus einen String.
; Der Parameter gamefield ist das Spielfeld.
; Der Parameter height ist ein Zähler für die derzeitige Höhe
; des Spielfelds, die von 0 bis zur Höhe des Spielfelds geht.
; count ist ein Zähler für die Spalten, der von 1 aus startet.
(: game-field (connect-n-game natural natural -> string))

(define game-field
  (lambda (gamefield height count)
    (cond ((= height (length (first gamefield))) "\n")
          ((cons? gamefield) 
           (string-append (add-tab-after (number->string count))
                          (game-line gamefield height)
                          (game-field gamefield (+ height 1) (+ count 1)))))))


; Nimmt eine Liste ung macht daraus einen String
; Da ein Spielfeld eine Liste von Listen ist müssen wir jede
; der inneren Liste in Strings umwandeln.
(: game-line ((list-of any) natural -> string)) 

(check-expect (game-line (list (list 0 0) (list 0 0)) 0) "0 0 \n")
(check-expect (game-line (list (list 0 0 0) (list 0 0 0)) 0) "0 0 \n")
(check-expect (game-line (list (list 1 2 1) (list 2 1 2)) 0) "1 2 \n")

(define game-line
  (lambda (gameline count)
    (cond ((empty? gameline) "\n")
          ((cons? gameline)
           (string-append
            (number->string (list-ref (reverse(first gameline)) count))
            " "
            (game-line (rest gameline) count))))))

; Überprüft ob in einer Spalte noch Platz für einen weiteren Chip ist.
; Erhält als Parameter das Spielfeld und die Spalte in die zuletzt, etwas
; eingeworfen wurde.
(: full-column? (connect-n-game natural -> boolean))

(check-expect (full-column? (make-connect-n-game 3 3) 2) #t)
(check-expect (full-column? 
               (insert-chip-in-game (make-connect-n-game 1 1) 1 1) 1) #f)
(check-expect (full-column? (list (list 1 1 1) (list 1 2 0)) 2) #t)

(define full-column?
  (lambda (gamefield round)
    (elem? 0 (list-ref gamefield (- round 1)))))

; Überprüft ob ein Spieler durch eine Reihe in einer simplen Liste
; gewonnen hat.
(: game-over-line ((list-of natural) natural natural natural -> boolean))

(check-expect (game-over-line (list 2 2 2 1 1 1 2 1 1 1 1) 4 1 0) #t)
(check-expect (game-over-line (list 2 2 2 1 1 1 2 1 1 1 2) 4 1 0) #f)
(check-expect (game-over-line (list 1 2 1 2 2 2 1) 3 2 0) #t)

(define game-over-line
  (lambda (gameline winning-requirement player count)
    (cond ((= count winning-requirement) #t)
          ((empty? gameline) #f)
          ((= player (first gameline)) 
           (game-over-line
            (rest gameline) winning-requirement player (+ count 1)))
          (else 
           (game-over-line
            (rest gameline) winning-requirement player 0)))))

; Überprüft ob ein Spieler durch eine Reihe in einer der Spielfeld-
; Spalten gewonnen hat.
; Erhält als Parameter das Spielfeld, die Bedingung für einen Sieg,
; den derzeitigen Spieler und die Spalte, in der zuletzt etwas eingeworfen
; wurde.
(: game-over-column (connect-n-game natural natural natural -> boolean))

(check-expect
 (game-over-column (list(list 1 1 1) (list 1 1 2) (list 1 2 1)) 3 1 1) #t)
(check-expect
 (game-over-column (list(list 1 1 1) (list 1 1 2) (list 1 2 1)) 3 2 1) #f)

(define game-over-column
  (lambda (gamefield winning-requirement player round)
    (game-over-line(list-ref gamefield (- round 1))
                   winning-requirement player 0)))

; Überprüft ob ein Spieler durch eine Reihe in einer der Spielfeld-
; reihen gewonnen hat.
; Erhält als Parameter das Spielfeld, die Bedingung für einen Sieg,
; den derzeitigen Spieler und die Spalte, in der zuletzt etwas eingeworfen
; wurde.
(: game-over-row (connect-n-game natural natural natural -> boolean))

(check-expect
 (game-over-row (list(list 1 1 1) (list 1 1 0) (list 1 1 2)) 3 1 2) #t)
(check-expect
 (game-over-row (list(list 1 1 0) (list 1 1 0) (list 1 1 0)) 3 2 2) #f)

(define game-over-row
  (lambda (gamefield winning-requirement player round)
    (game-over-line 
     (make-row-from-gamefield 
      gamefield (length 
                 (delete-elem 
                  0 (list-ref gamefield (- round 1)))))
     winning-requirement player 0)))

; Erhält ein connect-n-game und einen index und erstellt daraus eine Reihe des
; Spielfelds als Liste.
; Der Index gibt an welche Zeile des Spielfelds als Liste ausgegeben werden
; soll.
(: make-row-from-gamefield (connect-n-game natural -> (list-of %a)))

(check-expect
 (make-row-from-gamefield (list (list 1 2) (list 0 1)) 1) (list 1 0))
(check-expect
 (make-row-from-gamefield (list (list 1 2) (list 0 1)) 2) (list 2 1))

(define make-row-from-gamefield
  (lambda (gamefield index)
    (cond ((empty? gamefield) empty)
          ((cons? gamefield)
           (cons (list-ref (first gamefield) (if (= index 0) 0
                                                 (- index 1)))
                 (make-row-from-gamefield (rest gamefield) index))))))

; Erhält ein connect-n-game und einen index und erstellt die Diagonale ausgehend
; von der ersten Spalte ab dem gewählten Index.
(: make-diagonal-from-gamefield (connect-n-game natural -> (list-of %a)))

(check-expect
 (make-diagonal-from-gamefield (list (list 1 2) (list 0 1)) 0) (list 1 1))
(check-expect
 (make-diagonal-from-gamefield (list (list 1 2) (list 0 1)) 1) (list 2))

(define make-diagonal-from-gamefield
  (lambda (gamefield index)
    (cond ((empty? gamefield) empty)
          ((>= index (length(first gamefield))) empty)
          ((cons? gamefield)
           (cons (list-ref (first gamefield) index) 
                 (make-diagonal-from-gamefield
                  (rest gamefield) (+ index 1)))))))

; Ehält ein connect-n-game, einen Index und eine Position und erstellt die
; Diagonale als Liste ausgehend von der Position also die Spalte von der die 
; Diagonale ausgeht und den Index, also von welchem Punkt in der Spalte die 
; Diagonale startet.
(: make-diagonal-from-gamefield2 
   (connect-n-game natural natural -> (list-of %a)))

(check-expect
 (make-diagonal-from-gamefield2 (list (list 1 2) (list 0 1)) 0 0) (list 1 1))
(check-expect
 (make-diagonal-from-gamefield2 (list (list 1 2) (list 0 1)) 1 0) (list 0))

(define make-diagonal-from-gamefield2
  (lambda (gamefield position index)
    (cond ((empty? gamefield) empty)
          ((or (>= position (length gamefield)) 
               (>= index (length(first gamefield)))) empty)
          ((cons? gamefield)
           (cons (list-ref (list-ref gamefield position) index)
                 (make-diagonal-from-gamefield2 gamefield (+ position 1)
                                                (+ index 1)))))))

; Stellt fest ob das Spiel vorbei ist,
; also ob ein Spieler durch eine Reihe seiner Chips in einer Spalte, in einer
; Zeile, oder in einer Diagonale durch seinen letzten Einwurf gewonnen hat.
; Erhält als Parameter das Spielfeld, die Bedingung für einen Sieg,
; den derzeitigen Spieler und die Spalte, in der zuletzt etwas eingeworfen
; wurde.
(: game-over (connect-n-game natural natural natural -> boolean))

(check-expect
 (game-over (list(list 1 1 2) (list 1 1 0) (list 1 1 2)) 3 1 2) #t)
(check-expect
 (game-over (list(list 1 1 0) (list 1 1 0) (list 1 1 0)) 3 2 2) #f)
(check-expect
 (game-over (list(list 1 1 1) (list 1 1 2) (list 1 2 1)) 3 1 1) #t)

(define game-over
  (lambda (gamefield winning-requirement player round)
    (or (game-over-column gamefield winning-requirement player round)
        (game-over-row gamefield winning-requirement player round)
        (game-over-diagonal gamefield winning-requirement player round)
        (game-over-diagonal (reverse gamefield)
                            winning-requirement player round))))

; Prüft, ob keine Nullen mehr in den verschiedenen Spalten vorhanden sind
; also, ob das Spielfeld voll ist oder nicht.
(: connect-n-zero-check (connect-n-game -> boolean))

(check-expect (connect-n-zero-check (list (list 1 2 1) (list 2 1 2))) #t)
(check-expect (connect-n-zero-check (list (list 1 2 1) (list 2 1 0))) #f)

(define connect-n-zero-check
  (lambda (gamefield)
    (cond ((empty? gamefield) #t)
          ((elem? 0 (first gamefield)) #f)
          (else (connect-n-zero-check (rest gamefield))))))
                                       
; Prüft, ob der Spieler über eine Diagonale (sowohl von links nach rechts, als
; auch von rechts nach links) durch seinen letzten Einwurf gewonnen hat.
; Erhält als Parameter das Spielfeld, die Bedingung für einen Sieg,
; den derzeitigen Spieler und die Spalte, in der zuletzt etwas eingeworfen
; wurde.
(: game-over-diagonal (connect-n-game natural natural natural -> boolean))

(check-expect
 (game-over-diagonal (list (list 1 2 2) (list 2 1 2) (list 2 2 1)) 3 1 3) #t)
(check-expect
 (game-over-diagonal (list (list 1 2 2) (list 2 1 2) (list 2 2 0)) 3 2 3) #f)

(define game-over-diagonal
  (lambda (gamefield winning player round)
    (cond ((< round  (length (delete-elem 0 (list-ref gamefield (- round 1)))))
           (game-over-line  
            (make-diagonal-from-gamefield
             gamefield
             (- 
              (length (delete-elem 0 (list-ref gamefield (- round 1)))) round))
            winning player 0))
          ((>= round (length (delete-elem 0 (list-ref gamefield (- round 1)))))
           (game-over-line
            (make-diagonal-from-gamefield2
             gamefield
             (- round
                (length (delete-elem 0 (list-ref gamefield (- round 1))))) 0)
            winning player 0)))))
