module Matching (match) where
import Data.Maybe()
import Position()
import Substitution
import Term

-- vergleicht zwei Terme und liefert ggf eine Subst von 1. zu 2.
match :: Term -> Term -> Maybe Subst
match (Var v)           t2               = Just (single v t2) 
match (Comb _ _)        (Var _)          = Nothing -- Comb-Var kann es keine Subst
match c1@(Comb op1 t1s) c2@(Comb op2 t2s)
      | op1 /= op2 = Nothing -- falls name versch. gibt es keine Subst
      | c1 == c2   = Just (identity) -- falls gleich identity
      | otherwise  = match' t1s t2s (Just identity) -- mit match' in die Tiefe
--match' durchsucht Termlisten ob paarweise gemacht werden kann. 
--Außerdem benutzen wir einen  Akkumulator, um gefundene Subst zu composen 
 where
  match' _        _        Nothing  = Nothing -- Fail-Fall, der durchgereicht wird  
  match' []       (_:_)    (Just _) = Nothing
  match' (_:_)    []       (Just _) = Nothing
  match' []       []       ms       = ms -- Abbruch, Akku wird returned       
  match' (t1:t1s') (t2:t2s') (Just s) = case match t1 t2 of 
    Nothing        -> Nothing
    (Just newsubs) -> match' t1s' t2s' (Just(compose newsubs s))

--Testfälle zum durchprobieren, ob die Implementierung richtig ist.
--test1= apply (fromJust(match (Var "n") (Var "m"))) (Comb "add" [Var "n",Var "p"])
--test2= apply (fromJust(match (Var "n") (Comb "mul" [Var "x", Var "y"]))) (Comb "add" [Var "n",Var "p"])
--test3= apply (fromJust(match (Comb "mul" [Var "x", Var "y"]) (Var "n"))) (Comb "add" [Var "n",Var "p"])
--test4= apply (fromJust(match (Comb "mul" [Var "x", Var "y"]) (Comb "mul" [Var "x", Var "y"]))) (Comb "add" [Var "n",Var "p"])
--test5= apply (fromJust(match (Var "y") (Var "y"))) (Comb "add" [Var "n",Var "p"])
--test6= apply (fromJust(match (Comb "mul" [Var "x", (Comb "sub" [Var "y", Var "z"])]) (Comb "mul" [Var "x", Var "y"]))) (Comb "add" [Var "n",Var "p"])
       --Subst für x und Subst für y 
--test7= apply (fromJust(match (Comb "mul" [Var "x", Var "y"]) (Comb "mul" [(Comb "sub" [Var "1", Var "2"]), (Comb "sub2" [Var "3", Var "4"])]))) (Comb "add" [Var "x",Var "y"])
       --Subst x zuerst und dann Subst y (compose subst y subst x)
--test8= apply (fromJust(match (Comb "mul" [Var "x", Var "y"]) (Comb "mul" [(Comb "sub" [(Comb "sub2" [Var "3", Var "4"]), Var "2"]), (Comb "sub2" [Var "3", Var "4"])]))) (Comb "add" [Var "x",Var "y"])
--erst Subst für y und dann Subst für x ( compose subst x subst y)
--test9= apply (fromJust(match (Comb "mul" [Var "x", Var "y"]) (Comb "mul" [(Comb "sub" [Var "1", Var "2"]), (Comb "sub2" [(Comb "sub" [Var "1", Var "2"]), Var "4"])]))) (Comb "add" [Var "x",Var "y"])
