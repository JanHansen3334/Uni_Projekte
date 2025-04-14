module Position 
  ( selectAt
  , Pos
  , replaceAt
  , allPos
  , above
  , below
  , leftOf
  , rightOf
  ) where

import Term
import Substitution()

-- [] repräsentiert Wurzel 
type Pos = [Int]

-- nimmt einen Term und eine Position, liefert den Teilterm an der übergebenen
-- Position des Gesamtterms
selectAt :: Term -> Pos -> Term
selectAt t1              []     = t1
selectAt (Var _)         _      = error "Position nicht möglich" 
selectAt (Comb _ [])     _      = error "Position nicht möglich" 
selectAt (Comb _ (x:_))  (1:ys) = selectAt x ys
selectAt (Comb a (_:xs)) (y:ys) = selectAt (Comb a xs) ((y-1):ys)

--Tests:
-- selectAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) []
--Comb "mul" [Var "x",Comb "add" [Var "y",Var "z"]]
-- selectAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) [1]
--Var "x"
-- selectAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) [2]
--Comb "add" [Var "y",Var "z"]
-- selectAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) [2,1]
--Var "y"
-- selectAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) [2,2]
--Var "z"

-- ersetzt in Term (1. Argument) den durch die Position identifizierten 
-- Teilterm durch einen anderen Term (3. Argument)
replaceAt :: Term -> Pos -> Term -> Term
replaceAt _               []     t2 = t2
replaceAt (Var _)         _      _  = error "Position ist nicht möglich"
replaceAt (Comb _ [])     _      _  = error "Position ist nicht möglich"
replaceAt (Comb a (y:ys)) (1:xs) t2 = (Comb a ((replaceAt y xs t2):ys))
replaceAt (Comb a (y:ys)) (x:xs) t2 = (Comb a (y: (replaceAt1 ys (x:xs) t2 2))) 
 where
 -- die Hilfsfunktion wird mit pos 2 Aufgerufen, da wir die erste Position
 -- schon durchgegangen sind und jetzt die 2te betrachten. Im rekursiven 
 -- wird es dann hochgezählt.  
  replaceAt1 (y1:ys1) (x1:xs1) t2_1 pos = 
    if x1 == pos 
      then ((replaceAt y1 xs1 t2_1):ys1)
      else (y1: (replaceAt1 ys1 (x1:xs1) t2_1 (pos+1)))
  replaceAt1 []       _        _    _   = error "Position nicht möglich"
  replaceAt1 _        []       _    _   = error "Wird so nicht aufgerufen"

--Tests:
-- replaceAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) [2,2] (Comb "add" [Var "o", Var "p"])
--Comb "mul" [Var "x",Comb "add" [Var "y",Comb "add" [Var "o",Var "p"]]]
-- replaceAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) [2] (Comb "add" [Var "o", Var "p"])
--Comb "mul" [Var "x",Comb "add" [Var "o",Var "p"]]
-- replaceAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) [1] (Comb "add" [Var "o", Var "p"])
--Comb "mul" [Comb "add" [Var "o",Var "p"],Comb "add" [Var "y",Var "z"]]
-- replaceAt (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])]) [] (Comb "add" [Var "o", Var "p"])
--Comb "add" [Var "o",Var "p"]

-- liefert Liste aller in einem Term vorhandenen Positionen
-- unsere Positionen fangen mit [] für die Wurzel und 1 für das erste Element 
-- des Terms an.
allPos :: Term -> [Pos]
allPos (Var _)         = [[]]
allPos (Comb _ [])     = [[]]
allPos (Comb a (x:xs)) = [[]] ++ allPos1 (Comb a (x:xs)) 1 
 where
 -- die 1 Spiegelt die erste Position im Term dar.
   allPos1 (Var _) _         = [] --kann nicht auftreten (--Wall)
   allPos1 (Comb _ []) _     = []
   -- wenn wir eine weitere Tiefe haben, dann merken wir uns die vorherigen
   -- Positionen und fürgen sie an jedem Zweig vorne an.
   allPos1 (Comb a1 (x1:xs1)) y1 = (map (y1:) (allPos x1)) 
                                   ++ allPos1 (Comb a1 xs1) (y1 + 1) 

-- allPos (Comb "mul" [Var "x", (Comb "add" [Var "y", Var "z"])])
--[[],[1],[2],[2,1],[2,2]]
-- allPos (Var "z")
--[[]]
-- allPos (Comb "mul" [(Comb "add" [Var "y", Var "z"])])
--[[],[1],[1,1],[1,2]]

-- Prädikat das angibt, ob pos1 über pos2 steht
above:: Pos -> Pos -> Bool
above _         []      = False
above []        _       = True
above t1@(p1:p1s) t2@(p2:p2s) | t1 == t2  = False
                              | p1 /= p2  = False
                              | p1s == [] = p1==p2
                              | otherwise = above p1s p2s

-- Prädikat das angibt, ob pos1 unter pos2 steht 
below:: Pos -> Pos -> Bool
below p1 p2 = above p2 p1

--Tests:
-- above [2,1] [2,1]
--False
-- above [2,1] [2,2]
--False
-- above [2,1,1] [3,1,1,2]
--False
-- above [2,1,1] [3,1,2]
--False
-- above [2,1] [2,1,1]
--True
-- above [1,2] [1]
--False
-- above [] [2,1]
--True
-- above [1] []
--False
-- below [1,2] [1]
--True
-- below [1,2] [2]
--False
-- below [1,2] [1,2]
--False
-- below [1,2] [2,1,2]
--False
-- below [1,2,1] [2,2,1,1]
--False
--- below [1,2,1] [2,2]
--False
-- below [] [2,1]
--False
-- below [1] []
--True

-- Prädikat das angibt, ob pos1 links von pos2 steht 
leftOf:: Pos -> Pos -> Bool
leftOf []       _        = False
leftOf _        []       = False
leftOf t1@(p1:p1s) t2@(p2:p2s) | t1 == t2    = False                        
                               | above t1 t2 = False
                               | below t1 t2 = False
                               | p1 < p2     = True
                               | p1 == p2    = leftOf p1s p2s
                               | otherwise   = False

-- Prädikat das angibt, ob pos1 rechts von pos2 steht 
rightOf:: Pos -> Pos -> Bool
rightOf [] _  = False
rightOf _  [] = False
rightOf p1 p2 = leftOf p2 p1

--Tests:
-- leftOf [1] [1,2]
--False
-- leftOf [] [1,2]
--False
-- leftOf[2] [3,1,2]
--True
-- leftOf[2,2] [3,1,2]
--True
-- leftOf[2,2] [2,2,2]
--False
-- leftOf[2,2] [1,2,2]
--False
-- leftOf [1,2] [1]
--False

-- rightOf [1] [1,2]
--False
-- rightOf [] [1,2]
--False
-- rightOf [1] []
--False
-- rightOf [2] [3,1,2]
--False
-- rightOf [2,2] [3,1,2]
--False
-- rightOf [2,2] [2,2,2]
--False
-- rightOf [2,2] [1,2,2]
--True
