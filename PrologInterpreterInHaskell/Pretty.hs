module Pretty (Pretty, pretty) where
import Term

class Pretty a 
 where
  pretty :: a -> String

-- Stellt die Werte schön da.
instance Pretty Term 
 where
  -- returnen Var und Comb name
  pretty (Var name)              = name
  pretty (Comb name [])          = name
  -- returnt Comb von Var und Comb von comb auf der obersten Ebene
  pretty (Comb name [Comb x []]) = name ++ " " ++ x
  pretty (Comb name [Var x])     = name ++ " " ++ x
  -- fügt die Klammerung ein wenn Comb ein Elementig ist und der Inhalt ein Comb
  -- mit etwas anderem als [] ist. 
  pretty (Comb name [x])         = name ++ " " ++ "(" ++ pretty x ++ ")" 
  -- ruft unsere Hilfsfunktion auf, wenn wir mehr als ein Element in die Comb 
  -- Liste haben. Da wir nicht mehr auf der obersten Ebene sind muss mehr 
  -- geklammert werden als vorher.
  pretty (Comb name (x:xs))      = name ++ (pretty1 x) ++ (concatMap pretty1 xs) 
   where
    -- returnen Var und Comb name
    pretty1 (Var name1)               = " " ++ name1
    pretty1 (Comb name1 [])           = " " ++ name1
    -- returnt Comb von Var und Comb von comb angepasst auf eine tiefere Ebene.
    pretty1 (Comb name1 [Comb x1 []]) = " (" ++ name1 ++ " " ++ x1 ++ ")"
    pretty1 (Comb name1 [Var x1])     = name1 ++ " " ++ x1
    -- fügt die Klammerung ein
    pretty1 (Comb name1 [x1])         = name1 ++ " " ++ "(" ++ pretty1 x1 ++ ")"
    -- rekursiver Aufruf für innere Ebenen.
    pretty1 (Comb name1 (x1:xs1))     = " (" ++ name1 ++ (pretty1 x1) 
                                        ++ (concatMap pretty1 xs1) ++ ")"

--pretty (Var "m")
--"m"
-- pretty (Comb "Succ" [Comb "Zero" []])
--"Succ Zero"
-- pretty (Comb "add" [Comb "Succ" [Comb "Zero" []], Comb "mul" [Var "m", Var "n"]])
--"add (Succ Zero) (mul m n)"
-- pretty (Comb "Succ" [Comb "Succ" [Comb "Zero" []]])
--"Succ  (Succ Zero)"
--pretty (Comb "Succ"[Comb "Succ" [Comb "Succ" [Comb "Succ" [Comb "Zero" []]]]]) für Zeile 17
--"Succ (Succ (Succ (Succ Zero)))"

