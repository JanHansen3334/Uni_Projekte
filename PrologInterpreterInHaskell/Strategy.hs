module Strategy 
  ( Strategy
  , loStrategy
  , liStrategy
  , roStrategy
  , riStrategy
  , poStrategy
  , piStrategy
  , reduceWith
  , evaluateWith
  ) where
import Data.Maybe    
import Term
import Substitution()
import Position
import Prog
import Reduktion

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

-- LeftOuter Strategy
loStrategy :: Strategy
loStrategy prog term = [einfach (reduciblePos prog term) leftOf above]

-- LeftInner Strategy
liStrategy :: Strategy
liStrategy prog term = [einfach (reduciblePos prog term) leftOf below]

-- RightOuter Strategy
roStrategy :: Strategy
roStrategy prog term = [einfach (reduciblePos prog term) rightOf above]

-- RightInner Strategy
riStrategy :: Strategy 
riStrategy prog term = [einfach (reduciblePos prog term) rightOf below]

-- Hilffunktion die lo,ro, ri, li darstellt
-- es unterscheidet sich nur durch below/above left/right 
einfach:: [Pos] -> (Pos -> Pos -> Bool) -> (Pos -> Pos -> Bool) -> Pos
einfach []                _ _  = []
einfach [pos]             _ _  = pos  --die Zielposition (und Abbruchkriterium)
                               --wenn pos1 links bzw. rechts ist dann wird pos2 entfernt 
einfach (pos1:(pos2:ys)) lr ba | lr pos1 pos2 = einfach (pos1:ys) lr ba
                               --wenn pos1 above bzw. below ist dann wird pos2 entfernt 
                               | ba pos1 pos2 = einfach (pos1:ys) lr ba
                               --andernfalls muss pos 1 enfernt werden
                               | otherwise    = einfach (pos2:ys) lr ba 

--ParallelOuter Strategy
poStrategy :: Strategy
poStrategy prog term = par (reduciblePos prog term) [] above

--ParallelInner Strategy
piStrategy :: Strategy
piStrategy prog term = par (reduciblePos prog term) [] below

--Hilfsmethode für Parallel Strategien:
par :: [Pos] -> [Pos] -> (Pos -> Pos -> Bool) -> [Pos]
par []     xs    _   = xs  -- wenn a leer ist, ist xs unsere Ergebnisliste
--im ersten Aufruf wird das erste Element als bestes in den Akku eingefügt
par (a:as) []    ba  = par as [a] ba  
--wenn die zweite liste leer ist gab es in der Zwischenliste keine 
-- above oder below also ist a right oder left.
                    -- wenn a drüber bzw drunter ist dann gehört es ins Ergebnis
                    -- alle anderen Werte in xs sind links bzw, rechts von x also 
                    --auch rechts bzw. links von a
par (a:as) (x:xs) ba | ba a x    = par as (a:(par' [a] xs ba)) ba  
                    -- wenn a drunter bzw drüber ist wird a verworfen.
                     | ba x a    = par as (x:xs) ba  
                     -- ansonsten ist a neben x und es muss geprüft werden,
                     -- ob es neben einem der anderen Elemente ist.
                     | otherwise = par as (x:(par [a] xs ba)) ba  
 where
  par' (_:_:_) _     _   = []  --tritt nicht auf nur für (-Wall)
  par' []      xs1   _   = xs1  --tritt nicht auf nur für (-Wall)
  par' [_]     []    _   = []  --  Abbruchkriterium
  par' [a1] (x1:xs1) ba1 | ba1 a1 x1 = par' [a1] xs1 ba1 
                         | otherwise = (x1: (par' [a1] xs1 ba1))

-- Durchführung eines Reduktionsschrittes in Abhängigkeit einer Auswertungsstrategie durch eine Funktion.
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith strat prog term = oneStep (strat prog term) prog term 
 where
  oneStep []      _    _     = Nothing -- keine Reduktion möglich
  -- einelementige ist der abbruch, da [] nothing und nicht just returned
  oneStep [a]    prog1 term1 = reduceAt prog1 term1 a 
  oneStep (a:as) prog1 term1 = oneStep as prog1 (fromJust(reduceAt prog1 term1 a)) --rekursiver Aufruf

-- Auswertung eines Ausdrucks zu seiner Normalform in Abhängigkeit einer Auswertungsstrategie.
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog term | isNormalForm prog term = term
                             | otherwise              = evaluateWith strat prog (fromJust(reduceWith strat prog term))

--Testfälle:                             
--prog_1 = Prog [Rule (Comb "sqr" [Var "a"]) (Comb "mul" [Var "a", Var "a"]),Rule (Comb "mul2" [Var "a" ,Var "b"]) (Comb "add" [Var "a", Var "b", Var "a", Var "b"]),Rule (Comb "mul2" [Var "a", Var "b", Var "c"]) (Comb "add" [Var "a", Var "b", Var "c", Var "a", Var "b", Var "c"])]
--term_1 = Comb "add" [Comb "sqr" [Var "n"], Comb "sqr" [Comb "mul2" [Var "x", Var "z"]], Comb "add" [Var "i", Var "j", Comb "sqr" [Var "y"]]]
--term_2 = Comb "add" [Comb "sqr" [Comb "mul2" [Var "n", Var "m"]], Comb "sqr" [Var "y"], Comb "mul2" [Comb "mul2" [Var "x", Var "z"], Var "i", Comb "sqr" [Comb "mul2" [Var "j", Var "c"]]]]
--term_3 = Comb "sqr" [Comb "mul2" [Var "n", Var "m"]]

-- liStrategy prog_1 term_1
--[[1]]
-- loStrategy prog_1 term_1
--[[1]]
-- riStrategy prog_1 term_1
--[[3,3]]
-- roStrategy prog_1 term_1
--[[3,3]]
-- piStrategy prog_1 term_1
--[[3,3],[2,1]]
-- poStrategy prog_1 term_1
--[[2],[1]]
--V2 piStrategy prog_1 term_1
--[[1],[2,1],[3,3]]
--V2 poStrategy prog_1 term_1
--[[1],[2],[3,3]]

-- liStrategy prog_1 term_2
--[[1,1]]
-- loStrategy prog_1 term_2
--[[1]]
-- roStrategy prog_1 term_2
--[[3]]
-- riStrategy prog_1 term_2
--[[3,3,1]]
-- piStrategy prog_1 term_2
--[[3,3,1]]
-- poStrategy prog_1 term_2
--[[3],[2],[1]]
--V2 poStrategy1 prog_1 term_2
--[[1],[2],[3]]
--V2 piStrategy1 prog_1 term_2
--[[1,1],[2],[3,1],[3,3,1]]

-- liStrategy prog_1 term_3
--[[1]]
-- loStrategy prog_1 term_3
--[[]]
-- roStrategy prog_1 term_3
--[[]]
-- riStrategy prog_1 term_3
--[[1]]
-- piStrategy prog_1 term_3
--[[1]]
-- poStrategy prog_1 term_3
--[[]]

-- reduceWith liStrategy prog_1 term_2
--Just (Comb "add" [Comb "sqr" [Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "sqr" [Var "y"],Comb "mul2" [Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "mul2" [Var "j",Var "c"]]]])
-- reduceWith loStrategy prog_1 term_2
--Just (Comb "add" [Comb "mul" [Comb "mul2" [Var "n",Var "m"],Comb "mul2" [Var "n",Var "m"]],Comb "sqr" [Var "y"],Comb "mul2" [Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "mul2" [Var "j",Var "c"]]]])
-- reduceWith roStrategy prog_1 term_2
--Just (Comb "add" [Comb "sqr" [Comb "mul2" [Var "n",Var "m"]],Comb "sqr" [Var "y"],Comb "add" [Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "mul2" [Var "j",Var "c"]],Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "mul2" [Var "j",Var "c"]]]])
-- reduceWith riStrategy prog_1 term_2
--Just (Comb "add" [Comb "sqr" [Comb "mul2" [Var "n",Var "m"]],Comb "sqr" [Var "y"],Comb "mul2" [Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]])
-- reduceWith piStrategy prog_1 term_2
--Just (Comb "add" [Comb "sqr" [Comb "mul2" [Var "n",Var "m"]],Comb "sqr" [Var "y"],Comb "mul2" [Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]])
-- reduceWith poStrategy prog_1 term_2
--Just (Comb "add" [Comb "mul" [Comb "mul2" [Var "n",Var "m"],Comb "mul2" [Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "mul2" [Var "j",Var "c"]],Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "mul2" [Var "j",Var "c"]]]])
-- reduceWith poStrategy1 prog_1 term_2
--Just (Comb "add" [Comb "mul" [Comb "mul2" [Var "n",Var "m"],Comb "mul2" [Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "mul2" [Var "j",Var "c"]],Comb "mul2" [Var "x",Var "z"],Var "i",Comb "sqr" [Comb "mul2" [Var "j",Var "c"]]]])
-- reduceWith piStrategy1 prog_1 term_2
--Just (Comb "add" [Comb "sqr" [Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "mul2" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "sqr" [Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]])

-- evaluateWith liStrategy prog_1 term_2
--Comb "add" [Comb "mul" [Comb "add" [Var "n",Var "m",Var "n",Var "m"],Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]],Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]]
-- evaluateWith loStrategy prog_1 term_2
--Comb "add" [Comb "mul" [Comb "add" [Var "n",Var "m",Var "n",Var "m"],Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]],Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]]
-- evaluateWith roStrategy prog_1 term_2
--Comb "add" [Comb "mul" [Comb "add" [Var "n",Var "m",Var "n",Var "m"],Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]],Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]]
-- evaluateWith riStrategy prog_1 term_2
--Comb "add" [Comb "mul" [Comb "add" [Var "n",Var "m",Var "n",Var "m"],Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]],Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]]
-- evaluateWith piStrategy prog_1 term_2
--Comb "add" [Comb "mul" [Comb "add" [Var "n",Var "m",Var "n",Var "m"],Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]],Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]]
-- evaluateWith poStrategy prog_1 term_2
--Comb "add" [Comb "mul" [Comb "add" [Var "n",Var "m",Var "n",Var "m"],Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]],Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]]
-- evaluateWith poStrategy1 prog_1 term_2
--Comb "add" [Comb "mul" [Comb "add" [Var "n",Var "m",Var "n",Var "m"],Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]],Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]]
-- evaluateWith piStrategy1 prog_1 term_2
--Comb "add" [Comb "mul" [Comb "add" [Var "n",Var "m",Var "n",Var "m"],Comb "add" [Var "n",Var "m",Var "n",Var "m"]],Comb "mul" [Var "y",Var "y"],Comb "add" [Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]],Comb "add" [Var "x",Var "z",Var "x",Var "z"],Var "i",Comb "mul" [Comb "add" [Var "j",Var "c",Var "j",Var "c"],Comb "add" [Var "j",Var "c",Var "j",Var "c"]]]]
