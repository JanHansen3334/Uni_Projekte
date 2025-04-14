module Beispiele where

data Peano = Zero | Succ Peano

add :: Peano -> Peano -> Peano
add Zero     m = m
add (Succ n) m = Succ (add n m)

mul :: Peano -> Peano -> Peano
mul Zero     _ = Zero
mul (Succ n) m = add (mul n m) m

-- Zero
f :: Peano -> Peano
f _ = mul Zero (Succ (Succ Zero))

-- Endlosschleife
h :: Peano -> Peano
h x  = add (h x) (Succ Zero)

-- ruft Endlosschleife auf
g :: Peano -> Peano
g x = mul x (add (h (Succ (Succ Zero))) (Succ Zero))

-- terminiert für Zero an 2. Stelle, aber nicht für Succ _
y :: Peano -> Peano -> Peano
y _ Zero     = (Succ (Succ Zero))
y _ (Succ _) = h (Succ (Succ Zero))

-- terminiert für Zero an 1. Stelle, aber nicht für Succ _
z :: Peano -> Peano -> Peano
z Zero     _ = (Succ (Succ (Succ (Succ Zero))))
z (Succ _) _ = h (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

-- terminiert für Zero an 2v3 Stelle, aber nicht für Succ _
a :: Peano -> Peano -> Peano -> Peano
a _ Zero     _ = (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))))
a _ (Succ _) _ = h (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))

onlyOuter = f (h (Succ Zero))
-- nur outer: f (g (h (Succ Zero)))
--Zero
rOnotLO = y (h (Succ Zero)) (f (h (Succ Zero)))
-- RO aber nicht LO: y (h (Succ Zero)) (f (h (Succ (Succ Zero))))
--Succ (Succ Zero)
lOnotRO = z (f (h (Succ Zero))) (h (Succ Zero))
-- LO aber nicht RO: z (f (h (Succ (Succ (Succ (Succ Zero)))))) (h (Succ (Succ (Succ Zero))))
--Succ (Succ (Succ (Succ Zero)))
pOnotLORO = a (h (Succ Zero)) (f (h (Succ Zero))) (h (Succ Zero))
-- PO aber nicht LO,RO: a (h (Succ Zero)) (f (h (Succ (Succ Zero)))) (g (h (Succ (Succ (Succ (Succ Zero))))))
--Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))
never = h (Succ Zero)
-- nicht LO,RO,PO: g (h (Succ (Succ (Succ (Succ Zero))))) (getestet terminiert nie)
