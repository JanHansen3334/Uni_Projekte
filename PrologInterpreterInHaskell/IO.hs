module IO (main) where
import Data.List
import System.FilePath
import Parser
import Strategy
import Prog
import Term()
import Pretty

-- Hauptfunktion zum Initialisieren und Abschließen der Umgebung
main :: IO ()
main = do 
  putStr welcome
  commandloop loStrategy "" emptyProg --Initial mit leftOuter und emptyProg
    
-- Funktion, die so lange immer wieder aufgerufen wird, bis ":q" eingegeben wird
commandloop :: Strategy -> FilePath -> Prog -> IO ()
commandloop strat path prog= do
    putStr ((takeBaseName path) ++ "> ") --Gebe Filenamen> aus
    command <- getCommand path
    execCommand command
 where 
  -- Help
  execCommand ":h"                     = putStrLn helpmenu 
                                         >> commandloop strat path prog 
  execCommand ":help"                  = execCommand ":h" 
  -- Quit
  execCommand ":q"                     = return ()   
  execCommand ":quit"                  = execCommand ":q"
  -- Reload ruft einfach nochmal load auf mit dem alten Path
  execCommand ":r"                     = if null path  
                                          then do putStrLn "no Module to reload" 
                                                  commandloop strat path prog 
                                          else execCommand (":l " ++ path) 
  execCommand ":reload"                = execCommand ":r"
  -- Unload läd wieder das emptyProgramm 
  execCommand ":l"                     = if null path  
                                          then do putStrLn "no Module to unload" 
                                                  commandloop strat path prog 
                                          else do putStrLn "Ok, no Module loaded"
                                                  commandloop strat "" emptyProg 
  execCommand ":load"                  = execCommand ":l"
  -- Wechseln der Strategie
  execCommand (':':'s':'e':'t':xs)     = execCommand (":s" ++ xs)
  execCommand (':':'s':xs)             = putStrLn ("Strategy changed to" ++ xs) 
                                         >> case xs of  
    " lo" -> commandloop loStrategy path prog
    " ro" -> commandloop roStrategy path prog
    " li" -> commandloop liStrategy path prog
    " ri" -> commandloop riStrategy path prog
    " po" -> commandloop poStrategy path prog
    " pi" -> commandloop piStrategy path prog
    _     -> commandloop strat      path prog
  -- load File
  execCommand (':':'l':'o':'a':'d':xs) = execCommand (":l" ++ xs)
  execCommand (':':'l':xs)             = loadFile (tail xs) strat path prog 
  -- Expression auswerten
  execCommand expre                    = case (parse expre) of  
    (Left v)     -> putStrLn v >> commandloop strat path prog
    (Right term) -> putStrLn (pretty (evaluateWith strat prog term)) 
                    >> commandloop strat path prog

-- Lädt ein neues Programm das durch einen Path angegeben ist    
loadFile :: FilePath -> Strategy -> FilePath -> Prog -> IO ()
loadFile newpath strat oldpath prog = do res <- parseFile newpath 
                                         checkres res 
 where
  checkres (Left v)        = putStrLn v >> commandloop strat oldpath prog --Fehlerfall
  checkres (Right newprog) = do putStrLn "new Module loaded" 
                                commandloop strat newpath newprog --Erfolgsfall

-- liest so lange Zeilen ein, bis ein gültiges Kommando eingegeben wurde
getCommand :: FilePath -> IO String
getCommand path = do
  input <- getLine
  if isValidCommand input
    then return (input)
    else do 
      putStrLn "Invalid command. Refere to help, if needed" 
      putStr ((takeBaseName path) ++ "> ")
      getCommand path

-- Ein Prädikat, das angibt, ob ein ein Command valid ist oder nicht
isValidCommand:: String -> Bool
isValidCommand [] = False 
isValidCommand s@(x:_) | isPrefixOf ":l "    s = True -- :l ... ist True
                       | isPrefixOf ":load " s = True 
                        -- falls ":" erstes Zeichen, prüfen ob in Liste
                       | x == ':'              = elem s validCommands
                       -- ansonsten ist es ein Term, der eingegeben werden kann 
                       | otherwise             = True  

validCommands :: [String] -- Eine Liste mit möglichen Commands
validCommands = [":h", ":help", ":l", ":load", ":q", ":quit", ":r", ":reload", 
                 ":s lo", ":s li", ":s ro",":s ri", ":s po", ":s pi", 
                 ":set lo", ":set li", ":set ro",":set ri", ":set po", ":set pi"]

helpmenu :: String
helpmenu = unlines
  ["Commands available from the prompt:"
    ,"<expression>       Evaluates the specified expression."
    ,":h[elp]            Shows this help message."
    ,":l[oad] <file>     Loads the specified file."
    ,":l[oad]            Unloads the currently loaded file."
    ,":r[eload]          Reloads the lastly loaded file."
    ,":s[et] <strategy>  Sets the specified evaluation strategy"
    ,"                   where <strategy> is one of 'lo', 'li',"
    ,"                   'ro', 'ri', 'po', or 'pi'. Default is lo"
    ,":q[uit]            Exits the interactive environment."  ]

-- Diese Konstante wird benutzt um das Prog im "kein Prog geladen" zu repräsentieren    
emptyProg:: Prog
emptyProg = Prog []

welcome :: String
welcome = unlines
  [ "Welcome to Simple Haskell!"
    ,"Type ':help' for help."]
