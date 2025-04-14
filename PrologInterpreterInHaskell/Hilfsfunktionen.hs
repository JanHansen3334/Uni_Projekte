
isPrefixOf:: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) | x==y = isPrefixOf xs ys
                         | otherwise = False
                        
                         

--Hilfsfunktion für Testfälle, um Substitution zu entpacken (derzeit noch kein fromJust genutzt)
extractSub:: Maybe Subst -> Subst
extractSub (Just s) = s
extractSub Nothing  = \v -> (Var "falsch") --Umgang mit Nothing ->Var wird "Falsch"
