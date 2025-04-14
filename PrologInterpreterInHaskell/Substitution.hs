module Substitution (identity, apply, single, compose, Subst) where
import Term

-- eine Substitution ist eine Funktion, die eine Variable in einen Term umwandelt
type Subst = VarName -> Term

-- Dies ist die Identitätssubstitutition, die einen Term unverändert lässt
identity :: Subst
identity = \v -> Var v

-- Möglichkeit eine Substitution anzugeben.
single:: VarName -> Term -> Subst
single v t1 = \v1 -> if v == v1 
                       then t1 
                       else Var v1

-- apply wendet eine Substitution auf einen Term an.
-- Sie ersetzt also alle Vorkommen der Variable die substituiert wird.
apply :: Subst -> Term -> Term
apply s1 (Var v)     = s1 v
apply s1 (Comb c ts) = Comb c (map (apply s1) ts) 

-- compose verbindet zwei subst. Hierbei wird die 2. zuerst angewendet.
compose :: Subst -> Subst -> Subst
compose s2 s1 = \v -> apply s2 (s1 v)

-- apply (compose (single "m" (Var "o")) (single "n" (Var "m"))) (Comb "add" [Comb "Succ" [Comb "Zero" []], Var "n"])
--Comb "add" [Comb "Succ" [Comb "Zero" []],Var "o"]
-- apply (compose (single "n" (Var "m")) (single "m" (Var "o"))) (Comb "add" [Comb "Succ" [Comb "Zero" []], Var "n"])
--Comb "add" [Comb "Succ" [Comb "Zero" []],Var "m"]
-- apply(single "n" (Var "m"))(apply (single "m" (Var "o")) (Comb "add" [Comb "Succ" [Comb "Zero" []], Var "n"])

-- apply(single "n" (Var "m"))(apply (single "m" (Var "o")) (Comb "add" [Comb "Succ" [Comb "Zero" []], Var "n"]))
--Comb "add" [Comb "Succ" [Comb "Zero" []],Var "m"]

-- apply(single "m" (Var "o"))(apply (single "n" (Var "m")) (Comb "add" [Comb "Succ" [Comb "Zero" []], Var "n"]))
--Comb "add" [Comb "Succ" [Comb "Zero" []],Var "o"]

--apply identity t1 = t1
--apply (single v t1)(Var v) = t1
--apply s1(Comb c[t1,...,tn]) = Comb c[apply s1 t1, ..., apply s1 tn]
--apply(compose identity s1) t1 = apply (compose s1 identity) t1
--apply (compose s2 s1) t1 = apply s2 (apply s1 t1)
