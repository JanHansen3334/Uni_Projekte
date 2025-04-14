module Reduktion (findRule, reduceAt, reduciblePos, isNormalForm) where
import Data.Maybe
import Prog
import Term
import Matching
import Substitution
import Position

-- sucht die erste Regel aus Prog, die auf Term angewendet werden kann
findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])                           _            = Nothing
-- nicht möglich,da für Var keine Regel ex
findRule (Prog (Rule (Var _) _:_))           _            = Nothing 
-- Var kein Comb also keine Regel
findRule _                                   (Var _)      = Nothing 
-- es wird mit match geprüft, ob die erste Regel auf den Term angewendet werden kann.
-- wenn ja, dann return wir diesen, ansonsten prüfen wir die nächste Regel 
findRule (Prog ((Rule l@(Comb _ _) rhs):rs)) t@(Comb _ _) = case (match l t) of 
  (Nothing)   -> findRule (Prog rs) t
  (Just subs) -> Just (rhs, subs)

-- Test:
-- findRule (Prog [Rule (Comb "squ" [(Var "a"),(Var "n")]) (Comb "mul" [(Var "a"),(Var "a"), (Var "n"), (Var "n")])]) (Comb "add" [(Var "a")])

--versucht, einen Term an einer gegebenen Position zu reduzieren  
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt p t pos = case findRule p (selectAt t pos) of
  Nothing             -> Nothing --falls keine Regel passt Nothing
  -- ansonsten wenden wir Subst auf die RHS an setzen Ergebnis passend ein
  (Just (rhs, subst)) -> Just (replaceAt t pos (apply subst rhs)) 

-- reduceAt (Prog [Rule (Comb "squ" [(Var "a")]) (Comb "mul" [(Var "a"),(Var "a")])]) (Comb "add" [(Var "a")]) []
--Nothing
-- reduceAt (Prog [Rule (Comb "squ" [(Var "a")]) (Comb "mul" [(Var "a"),(Var "a")])]) (Comb "add" [(Var "a")]) [1]
--Nothing
-- reduceAt (Prog [Rule (Comb "squ" [(Var "a")]) (Comb "mul" [(Var "a"),(Var "a")])]) (Comb "add" [(Var "a"),(Comb "mul" [Var "o"])]) [1]
--Nothing
-- reduceAt (Prog [Rule (Comb "squ" [(Var "a")]) (Comb "mul" [(Var "a"),(Var "a")])]) (Comb "add" [(Var "a"),(Comb "mul" [Var "o"])]) [2]
--Nothing
-- reduceAt (Prog [Rule (Comb "squ" [(Var "a")]) (Comb "mul" [(Var "a"),(Var "a")])]) (Comb "add" [(Var "a"),(Comb "squ" [Var "o"])]) [2]
--Just (Comb "add" [Var "a",Comb "mul" [Var "o",Var "o"]])

--gibt eine Liste aller Positionen eines Terms zurück, an denen der Term reduziert werden kann
reduciblePos :: Prog -> Term -> [Pos]
--wir erzeugen eine Liste alle Positionen und prüfen ob sie reduzierbar sind.
reduciblePos p t = reducible' p t (allPos t) []
  where reducible' _  _  []     bs = reverse bs
        reducible' p1 t1 (a:as) bs | isJust(reduceAt p1 t1 a) = reducible' p1 t1 as (a:bs)
                                   | otherwise                = reducible' p1 t1 as bs

-- Test:
-- reduciblePos (Prog [Rule (Comb "squ" [(Var "a")]) (Comb "mul" [(Var "a"),(Var "a")])]) (Comb "add" [(Var "a"),(Comb "squ" [Var "o"])])
--[[2]]

-- bestimmt, ob ein Term in Normalform ist. Wir prüfen, ob eine Reduktion möglich ist.
isNormalForm :: Prog -> Term -> Bool
isNormalForm prog term = null (reduciblePos prog term)

-- isNormalForm (Prog [Rule (Comb "squ" [(Var "a")]) (Comb "mul" [(Var "a"),(Var "a")])]) (Comb "add" [(Var "a"),(Comb "mul" [Var "o"])])
--True
-- isNormalForm (Prog [Rule (Comb "squ" [(Var "a")]) (Comb "mul" [(Var "a"),(Var "a")])]) (Comb "add" [(Var "a"),(Comb "squ" [Var "o"])])
--False
