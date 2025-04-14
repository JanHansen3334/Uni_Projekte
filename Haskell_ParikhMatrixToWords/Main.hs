module Main where

--cabal run myapp to compile
--ghci app/Main.hs to open

import Math.Combinat
import Data.List
--import Data.Matrix
--import Data.Set

-- partitionsWithKParts 4 10 (sum=10 and length 4)

main :: IO ()
main = putStrLn "Hello, Haskell!"

--------------------------------------------------------------------------------

--shuffle
inserts :: [a] -> [a] -> [[a]]
inserts [] ys = [ys]
inserts xs [] = [xs]
inserts xs@(x:xt) ys@(y:yt) = [x:zs | zs <- inserts xt ys] ++ [y:zs | zs <- inserts xs yt]

--Permutations with double occurences
uniquePermutations :: Ord a => [a] -> [[a]]
uniquePermutations = foldr (concatMap . inserts) [[]] . group . sort

--shuffle a word to each word in a word set
setShuffle42 :: [String] -> String -> [String]
setShuffle42 [] _ = []
setShuffle42 [y] x = inserts y x
setShuffle42 (y:ys) x = (inserts y x) ++ (setShuffle42 ys x)
--setShuffle42 [y] x = shuffle42 y x
--setShuffle42 (y:ys) x = (shuffle42 y x) ++ (setShuffle42 ys x)


-- shuffle 2nd word in 1st word
--shuffle two words
shuffle42 :: String -> String -> [String]
--shuffle42 [] xs = [xs]
--shuffle42 ys [] = [ys]
shuffle42 xs ys = inserts xs ys
--shuffle42 ys [x] = shuffleA42 ys x (shuffleD42 [] (shuffleB42 1) (length ys))
--shuffle42 ys (x:xs) = shuffleA42 ys x (shuffleD42 [] (shuffleB42 (length (x:xs))) (length ys))
--shuffle42 _ _ = ["fail42"]

--get all shuffle positions 
shuffleD42 :: [[Int]] -> [Int] -> Int -> [[Int]]
shuffleD42 xs [y] z | y == z = xs ++ [[y]]
                    | otherwise = shuffleD42 (xs ++ [[y]]) [y+1] z
shuffleD42 xs (y:ys) z | (last ys) == z = xs ++ [(y:ys)]
                       | y==z = shuffleD42 (xs ++ [(y:ys)]) (shuffleE42 (y:ys) z) z
                       | otherwise = shuffleD42 (xs ++ [(y:ys)]) ((y+1):ys) z
shuffleD42 _ _ _ = [[404]]

--increase the list at the correct position
shuffleE42:: [Int] -> Int -> [Int]
shuffleE42 [y] z | y == z = [404]
                 | otherwise = [y+1]
--shuffleE42 (y:[y2]) z | y==z && y2==z = ((y2):[y2])
--                      | y==z = ((y2+1):[y2+1])
--                      | otherwise = ((y+1):[y2]) 
shuffleE42 (y:y2:ys) z | y==z && y2==z = shuffleF42 (y:y2:ys) z 0 []
shuffleE42 (y:y2:ys) z | y == z = ((y2+1): (y2+1):ys)
                    | otherwise = ((y+1):ys) 
shuffleE42 _ _ = [408]

shuffleF42 :: [Int] -> Int -> Int -> [Int] -> [Int]
shuffleF42 (y:ys) z i xs | z == y = shuffleF42 ys z (i+1) xs
                         | i == 0 =  xs ++ ((y+1):ys)
                         | otherwise = shuffleF42 (y:ys) z (i-1) ((y+1):xs)
shuffleF42 _ _ _ _ = [808]

-- get smallest shuffle positions 
shuffleB42 :: Int -> [Int]
shuffleB42 0 = []
shuffleB42 x = [0] ++ shuffleB42 (x-1)  

--go through all position lists to get a word
shuffleA42 :: String -> Char -> [[Int]] -> [String]
shuffleA42 ys x [z] = shuffleC42 [] 0 ys x (reverse z)
shuffleA42 ys x (z:zs) = (shuffleC42 [] 0  ys x (reverse z)) ++ (shuffleA42 ys x zs)
shuffleA42 _ _ _ = ["failA42"]

--change position list to word
shuffleC42 :: String -> Int -> String -> Char -> [Int] -> [String]
shuffleC42 yys _ yss _ [] = [yys ++ yss]
shuffleC42 yys pos [] x (z:zs) | z== pos = shuffleC42 (yys ++ [x]) pos [] x zs
shuffleC42 yys pos (ys:yss) x [z] | z == pos = [yys ++ [x] ++ (ys:yss)]
                                  | otherwise = shuffleC42 (yys ++ [ys]) (pos +1) yss x [z] 
shuffleC42 yys pos (ys:yss) x (z:zs) | z == pos = shuffleC42 (yys ++ [x]) pos (ys:yss) x zs
                                     | otherwise = shuffleC42 (yys ++ [ys]) (pos +1) yss x (z:zs) 
shuffleC42 _ _ _ _ _ = ["failC42"]
{-shuffle42 :: String -> String -> [String]
shuffle42 [] [] = []
shuffle42 [] x  = [x]
shuffle42 x []  = [x]
shuffle42 ys [x] = shuffle42' [] ys x []
shuffle42 ys xs = shuffle43 ys xs -1 (length xs) (length ys)

shuffle43 :: String -> String -> Int -> Int -> Int -> [String]
shuffle43 ys [x] z 1 iy = shuffle42' [] ys x []  
shuffle43 ys (x:xs) z ix iy = (shuffle43 (ys ++ xs) x (z+1) 1 (iy +ix -1))++ 
 
			
-- shuffle in letter in a word			
shuffle42':: String -> String -> Char -> [String] -> [String] 
shuffle42' [] [] x1 [] = [x1]
shuffle42' ys [z] x1 zys | x1 == z = zys ++ [ys++ [x1] ++ [z]]
			 | otherwise = zys ++ [ys++ [x1] ++ [z]] ++ [ys++ [z] ++ [x1]]
shuffle42' ys (z:zs) x1 zys | x1 == z = shuffle42' (ys++ [z]) zs x1 zys
			    | otherwise = shuffle42' (ys++ [z]) zs x1 (zys ++ [ys++ [x1] ++ (z:zs)])
-}
--------------------------------------------------------------------------------
--ParikMatrixValues for a given alphabet length maximal 10
--parikhMatrixValues :: String -> Int -> [Int]

--ParikhMatrix for a word list of columns         
--parikhMatrixFromWord :: String -> [[Int]]   

--for alphabet size all values letters to the highest (max 10) is needed
--parikhMatrixElemFromWord :: String -> [Int]            

--calculate highest letter in a word alphabetical order (up to 10 j)
--alphabetLengthAlphabetical :: String -> Int

--Takes a list of Partitions and convert them into binary strings (the number of a's and b's must be given                                                                
--partitionListToString :: [Partition] -> Int -> Int -> [String]

--makes from partitionToString a single String
--partitionToStringConcat :: Partition -> Int -> Int -> String

--partition in binary string a and b
--convert a given Partition in a string with given number of a's and b's 
--partitionToString :: Partition -> Int -> Int -> [String]                    

-- gives all partition with <=k parts, n number (value input of partitionfunction) (P(42))
--groupedPartitionsWithKParts :: Int -> Int -> [Partition]                                   
                                   
-- returns the Partitionlist with a max element lower then the given int                                   
--partitionLowestMaxElem :: [Partition] -> Int -> [Partition]                                   
                                   
-- finds the max Element of a given Partition and max value 0                                  
-- partitionMaxElem :: Partition -> Int -> Int                                   

-- given number n possible partitions with max element k and and max length l (number of b's)
-- partitionMaxElemMaxLength :: Int -> Int -> Int -> [Partition]

--greedy result calculate all permutations of a String m! and delete double elements
--greedyAlg :: String -> [String]

--indexTable for a given alphabet length maximal 10
-- a,b,ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij
--position (the position number is not in the table it's equivalent to the row)
--0
--1
--2
-- vdots
--length of String
-- returns for a given word his indextable
--indexTableWord :: String -> (String,[[Int]])

-- return the column x of the indexTable of the word s. The first 0 is the initial from position 0
--getColumnIndexTable :: String -> Int -> [Int]

--get the Column y of a given pair of string and indexTable
--getColumnIndexTableHelp :: Int -> (String,[[Int]]) -> [Int]

-- calculates for a column of the indexTable where their indexes align
--indexPositions :: [Int] -> [(Int, Int, Int)]
--              column-list   value, firstpos, lastpos

-- convert all Partitions to a list of possible Positions
--partitionToIndexPositions :: String -> Int -> Int -> Int -> Int -> Int -> [[(Int,Int,Int)]] 

--gives allcorrect results and delete the one with the wrong list
--intersection of position lists between like bc results and abc results 
--Int value is the number of c or the number of letters we wanna insert
--intersectPosList :: [[[(Int,Int)]]] -> Int -> [[(Int, Int)]]

--shuffle a word to another word and delete double occurences
--first argument given word and 2nd the word/letter to shuffle in
--shuffleWords :: String -> String -> [String]

--maximal 10 first 3
--mEquivalentWords :: String -> [String]
--------------------------------------------------------------------------------
               
-- check if a word in a list of words has the same Parikh matrix
greedy4 :: String -> [String]
greedy4 word = greedy3 (uniquePermutations word) (parikhMatrixElemFromWord word)
--greedy4 :: [String] -> String -> [String]
--greedy4 s1 word = greedy3 s1 (parikhMatrixElemFromWord word)

-- check if a word in a list of words has the same Parikh matrix
greedy3 :: [String] -> [Int] -> [String]
greedy3 [] _ = []
greedy3 [x] y | (parikhMatrixElemFromWord x) == y = [x]
              | otherwise = []
greedy3 (x:xs) y | (parikhMatrixElemFromWord x) == y = (x: (greedy3 xs y))
                 | otherwise = greedy3 xs y


-- greedy shuffle
--------------------------------------------------------------------------------
--greedy result
greedy2 :: String -> String -> String -> [String]
--greedy2 s1 s2 word = greedy2' (shuffleWords s1 s2) (parikhMatrixElemFromWord word) where
greedy2 s1 s2 word = greedy2' (shuffle42 s1 s2) (parikhMatrixElemFromWord word) where
                 greedy2' :: [String] -> [Int] -> [String]
                 greedy2' [] _ = []
                 greedy2' [x] y | (parikhMatrixElemFromWord x) == y = [x]
                                | otherwise = []
                 greedy2' (x:xs) y | (parikhMatrixElemFromWord x) == y = (x: (greedy2' xs y))
                                   | otherwise = greedy2' xs y

-- greedy shuffle (permutations)
--------------------------------------------------------------------------------
--greedy result
greedyAlg :: String -> [String]
greedyAlg s = greedyAlg' (nub (Data.List.permutations s)) (parikhMatrixElemFromWord s) where
               greedyAlg' :: [String] -> [Int] -> [String]
               greedyAlg' [] _ = []
               greedyAlg' [x] y | (parikhMatrixElemFromWord x) == y = [x]
                                | otherwise = []
               greedyAlg' (x:xs) y | (parikhMatrixElemFromWord x) == y = (x: (greedyAlg' xs y))
                                   | otherwise = greedyAlg' xs y
--greedy1                                   
greedy1 :: String -> [String]
greedy1 s = greedy1' (Data.List.permutations s) (parikhMatrixElemFromWord s) where
               greedy1' :: [String] -> [Int] -> [String]
               greedy1' [] _ = []
               greedy1' [x] y | (parikhMatrixElemFromWord x) == y = [x]
                                | otherwise = []
               greedy1' (x:xs) y | (parikhMatrixElemFromWord x) == y = (x: (greedy1' xs y))
                                   | otherwise = greedy1' xs y
--------------------------------------------------------------------------------  
-- use nub instead                                   
--removeDuplicatePerm :: [String] -> [String] -> [String]
--removeDuplicatePerm [] xs = xs
--removeDuplicatePerm [x] xs | elem x xs = xs
--                           | otherwise = [x] ++ xs
--removeDuplicatePerm (y:ys) xs | elem y xs = removeDuplicatePerm ys xs
--                              | otherwise = removeDuplicatePerm ys ([y] ++ xs)                           


--------------------------------------------------------------------------------
-- * Partitions with given number of parts

-- | Lists partitions of @n@ into @k@ parts.
--
-- > sort (partitionsWithKParts k n) == sort [ p | p <- partitions n , numberOfParts p == k ]
--
-- Naive recursive algorithm.
--
{-partitionsWithKPartsMaxValue 
  :: Int    -- ^ @k@ = number of parts (length or new sigma)
  -> Int    -- ^ @v@ = biggest value of the partition (k_i known)
  -> Int    -- ^ @n@ = the integer we partition
  -> [Partition]
partitionsWithKPartsMaxValue k v n = map Partition_ $ go n k n where -- change from here on
{-
  h = max height
  k = number of parts
  n = integer
-}
  go !h !k !n 
    | k <  0     = []
    | k == 0     = if h>=0 && n==0 then [[] ] else []
    | k == 1     = if h>=n && n>=1 then [[n]] else []
    | otherwise  = [ a:p | a <- [1..(min h (n-k+1))] , p <- go a (k-1) (n-a) ]

--------------------------------------------------------------------------------
-}
--------------------------------------------------------------------------------
-- k parts, n number
groupedPartitionsWithKParts :: Int -> Int -> [Partition]
groupedPartitionsWithKParts k n    | k <  0     = []
                                   | k == 0     = if n==0 then (partitionsWithKParts k n) else []
                                   | otherwise  = (partitionsWithKParts k n) ++ (groupedPartitionsWithKParts (k-1) n)
                                   
                                   
-- returns the Partitionlist with a max element lower then the given int                                   
partitionLowestMaxElem :: [Partition] -> Int -> [Partition]                                   
partitionLowestMaxElem [] _ = []
partitionLowestMaxElem [(Partition xs)] y | (partitionMaxElem (Partition xs) 0) <= y = [Partition xs]
                                        | otherwise = [] 
partitionLowestMaxElem ((Partition xs):zs) y | (partitionMaxElem (Partition xs) 0) <= y = ((Partition xs): (partitionLowestMaxElem zs y))
                                        | otherwise = (partitionLowestMaxElem zs y)                                         
                                   
-- finds the max Element of a given Partition and max value 0                                  
partitionMaxElem :: Partition -> Int -> Int                                   
partitionMaxElem (Partition []) _ = 0
partitionMaxElem (Partition [x]) n | n < x = x
                                   | otherwise = n
partitionMaxElem (Partition (x:xs)) n | n < x = partitionMaxElem (Partition xs) x
                                      | otherwise = partitionMaxElem (Partition xs) n


-- given number n possible partitions with max element k and and max length l (number of b's)
partitionMaxElemMaxLength :: Int -> Int -> Int -> [Partition]
partitionMaxElemMaxLength n k l = partitionLowestMaxElem (groupedPartitionsWithKParts l n) k

-- result for 72 12 24: 894769 solutions in 5-6 min (wrong input)
-- result for 72 12 12: 61108  solutions in 20 seconds 
-- partitionMaxElemMaxLength 72 12 12 (21.15 secs, 7,136,009,400 bytes)
-- result for 72 12 12 with conversion: 42 seconds
-- partitionListToString (partitionMaxElemMaxLength 72 12 12) 12 12 (23.11 secs, 7,963,698,056 bytes)
--first time execution (29.40 secs, 7,963,701,880 bytes)

-- partitionMaxElemMaxLength 78 12 12 (30.01 secs, 11,779,413,904 bytes)
-- (31.53 secs, 12,533,832,488 bytes) result for 78 12 12: (18.41 secs, 10,298,128,792 bytes)
-- partitionListToString (partitionMaxElemMaxLength 78 12 12) 12 12 (31.53 secs, 12,533,832,488 bytes)
-- length(mEquivalentWords "abababababababababababab")
-- 57801
--(30.24 secs, 10,318,025,400 bytes)


--maximal 10 first 3
--mEquivalentWords "ccccccbbbbbbaaaaa"
--["ccccccbbbbbbaaaaa"]
--(0.05 secs, 213,728 bytes)
--["ccccccbbbbbbaaaaa"]
--(0.07 secs, 213,904 bytes)

--["ccccccbbbbbbaaaaa"]
--(0.00 secs, 209,904 bytes)



--mEquivalentWords "aaaaabbbbbbcccccc"
--["aaaaabbbbbbcccccc"]
--(13.81 secs, 6,353,433,512 bytes)
--["aaaaaabbbbbbcccccc"]
--(13.54 secs, 6,353,426,728 bytes)

--["aaaaaabbbbbbcccccc"]
--(24.66 secs, 6,353,433,912 bytes)



--mEquivalentWords "aabbccabccbaccbbaa"
-- 5652     5652
--(3.64 secs, 1,222,894,016 bytes)
--(5.49 secs, 1,129,479,280 bytes)

--greedy1 "jdibgchefa"
--(272.76 secs, 103,597,166,264 bytes)
--length(greedy1 "jdibgchefa")
--18694
--(288.92 secs, 103,436,711,080 bytes)

--Data.List.permutation "jdibgchefa"
--(361.61 secs, 32,991,797,872 bytes)
--length(Data.List.permutations "jdibgchefa")
--3628800
--(0.30 secs, 1,092,298,104 bytes)


--mEquivalentWords "jdibgchefa"
--(26.04 secs, 7,723,437,960 bytes)
--length(mEquivalentWords "jdibgchefa")
--18694
--(29.52 secs, 7,553,434,608 bytes)
--(25.29 secs, 7,553,434,600 bytes)


{-[                  1,
                   1,0,
                 3,3,0,
               2,1,1,0,
             3,5,2,2,0,
           1,3,5,2,2,0,
         2,0,0,0,0,0,0,
       1,2,0,0,0,0,0,0,
     2,0,0,0,0,0,0,0,0,
   1,0,0,0,0,0,0,0,0,0]-}
--length(mEquivalentWords "jdibigcgedccheefa")
--
--5520
--(35.85 secs, 18,134,666,888 bytes)

-- with mistake where i shuffled to less
--length(mEquivalentWords "jdibigacdddfeghiicgedccheefa")
--length(mEquivalentWords "dbacdddfecedcceefa")
--length(mEquivalentWords "jdidigdddddddghiidgddddhdddd")
--length(mEquivalentWords "dbgacdddfegcgedcceefa")
--54893775
--(1107.78 secs, 256,867,006,456 bytes)



--parikhMatrixValues "aabbccabccbaccbbaa" 3
--[6,6,18,6,18,42]


--mEquivalentWords "ccccccbbbbbbaaaaa"
--[5,6,0,6,0,0]
--partitionListToString (partitionMaxElemMaxLength 0 5 6) 5 6
--[""]

--mEquivalentWords "aaaaabbbbbbcccccc"
--["aaaaabbbbbbcccccc"]
--(13.81 secs, 6,353,433,512 bytes)
--[5,6,30,6,36,180]
--partitionListToString (partitionMaxElemMaxLength 30 5 6) 5 6
--["aaaaabbbbbb"]

 
--------------------------------------------------------------------------------

--maximal 10 first 3
mEquivalentWords :: String -> [String]
mEquivalentWords [] = []
mEquivalentWords s | (alphabetLengthAlphabetical s) == 2 = mHelp3 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   | (alphabetLengthAlphabetical s) == 3 = mHelp1 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   --cases Sigma>3--
                   | (alphabetLengthAlphabetical s) == 4 = mHelp4 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   | (alphabetLengthAlphabetical s) == 5 = mHelp5 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   | (alphabetLengthAlphabetical s) == 6 = mHelp6 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   | (alphabetLengthAlphabetical s) == 7 = mHelp7 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   | (alphabetLengthAlphabetical s) == 8 = mHelp8 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   | (alphabetLengthAlphabetical s) == 9 = mHelp9 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   | (alphabetLengthAlphabetical s) ==10 = mHelp10 (parikhMatrixValues s (alphabetLengthAlphabetical s))
                   |otherwise = ["mEquivalentWords more implementing"]




-- [4,4,8,2,4,4]
-- a b ab c bc abc

 --"baabbaab",
 --"babaabab",
 --"bbaaaabb"
 {-
indexNumbers (intersectPosList (partitionToIndexPositions "bbaaaabb" 2 4 4 8 4) 2)
[[(7,7,1),(1,1,1)]]
ghci> indexNumbers (intersectPosList (partitionToIndexPositions "babaabab" 2 4 4 8 4) 2)
[[(6,7,1),(1,2,1)]]
ghci> indexNumbers (intersectPosList (partitionToIndexPositions "baabbaab" 2 4 4 8 4) 2)
[[(4,4,2)],[(5,7,1),(1,3,1)]]
-}
--------------------------------------------------------------------------------





--productWord(regExp (compressionProductOfWordsForIndex(productOfWordsForIndex "abbabaab" (indexNumbers (intersectPosList (partitionToIndexPositions "abbabaab" 2 4 4 8 4) 2)))) "c")
--["abbccabaab","abbcacbaab","abbaccbaab"]


--regExp (compressionProductOfWordsForIndex(productOfWordsForIndex "abbabaab" (indexNumbers (intersectPosList (partitionToIndexPositions "abbabaab" 2 4 4 8 4) 2)))) "c"
--(s c b bc ab abc) c )))) "c" 
--[["abb"],["cca","cac","acc"],["baab"]]



--compressionProductOfWordsForIndex(productOfWordsForIndex "abbabaab" (indexNumbers (intersectPosList (partitionToIndexPositions "abbabaab" 2 4 4 8 4) 2)))
--[("abb",0),("a",2),("baab",0)]


--productOfWordsForIndex "abbabaab" (indexNumbers (intersectPosList (partitionToIndexPositions "abbabaab" 2 4 4 8 4) 2))
--[("a",0),("b",0),("b",0),("a",2),("baab",0)]
--------------------------------------------------------------------------------

--parikhMatrixValues "abbaccabba" 3
-- [4,4,8,2,4,4]
-- a b ab c bc abc

{-partitionListToString (partitionMaxElemMaxLength 8 4 4) 4 4
["aabbbbaa",
 "ababbaba",
 "abbaabba",
 "abbabaab",
 "baababba",
 "baabbaab",
 "babaabab",
 "bbaaaabb"]
-}
--partitionToIndexPositions s c b bc ab abc
--partitionToIndexPositions "aabbbbaa" 2 4 4 8 4
{-[[[(4,4),(4,4)],
    [(5,5),(3,3)],
    [(6,8),(0,2)]], [[(3,3),(3,3)],
                     [(4,4),(0,2)]]]
             empty             0
  partitionToIndexPositions "ababbaba" 2 4 4 8 4
[[[(4,4),(4,4)],
  [(5,6),(2,3)],
  [(7,8),(0,1)]],   [[(4,4),(2,3)]]]
            empty              0
  partitionToIndexPositions "abbaabba" 2 4 4 8 4
[[[(3,5),(3,5)],
  [(6,6),(2,2)],
  [(7,8),(0,1)]],   [[(3,5),(3,5)]]]
         [(3,5),(3,5)],   ccaa caca caac acca acac aacc  6
  partitionToIndexPositions "abbabaab" 2 4 4 8 4
[[[(3,4),(3,4)],
  [(5,7),(2,2)],
  [(8,8),(0,1)]],  [[(3,4),(3,4)],
                    [(5,7),(0,1)]]]
            [(3,4),(3,4)]     3
 partitionToIndexPositions "baababba" 2 4 4 8 4
[[[(4,5),(4,5)],
  [(6,6),(1,3)],
  [(7,8),(0,0)]],  [[(4,5),(4,5)]]]
            [(4,5),(4,5)]     3  ccb, cbc, bcc
  partitionToIndexPositions "baabbaab" 2 4 4 8 4
[[[(4,4),(4,4)],
  [(5,7),(1,3)],
  [(8,8),(0,0)]],  [[(4,4),(4,4)],
                    [(5,7),(0,3)]]]
     [(4,4),(4,4)],[(5,7),(1,3)]  10
  partitionToIndexPositions "babaabab" 2 4 4 8 4 
[[[(3,5),(3,5)],
  [(6,7),(1,2)],
  [(8,8),(0,0)]],  [[(6,7),(0,2)]]]
           [(6,7),(1,2)]    4
  partitionToIndexPositions "bbaaaabb" 2 4 4 8 4
[[[(2,6),(2,6)],
  [(7,7),(1,1)],
  [(8,8),(0,0)]],  [[(7,7),(0,6)]]]
           [(7,7),(1,1)]   1
      
                         27
-}
--------------------------------------------------------------------------------

--gives allcorrect results and delete the one with the wrong list
--intersection of position lists between like bc results and abc results 
--Int value is the number of c or the number of letters we wanna insert
intersectPosList :: [[[(Int,Int)]]] -> Int -> [[(Int, Int)]]
intersectPosList xs y = helpInterPosList (intersectPositionList xs) y

--[[(2,6)],[(7,7),(1,1)],[(0,0)]]
-- delete to small results
helpInterPosList :: [[(Int, Int)]] -> Int -> [[(Int, Int)]]
helpInterPosList [] _ = []
helpInterPosList [x] y | (length x) < y = []
                         | otherwise = [x]
helpInterPosList (x:xs) y | (length x) < y = [] ++ (helpInterPosList xs y)
                          | otherwise = [x] ++ (helpInterPosList xs y)

{-[[[(2,6),(2,6)],
  [(7,7),(1,1)],
  [(8,8),(0,0)]],  [[(7,7),(0,6)]]]
           -} 
--gets lists of possible positions and intersect them
intersectPositionList :: [[[(Int,Int)]]] -> [[(Int, Int)]]
intersectPositionList [[[]]] = []
intersectPositionList [] = []
intersectPositionList [[],_] = []
intersectPositionList [_,[]] = []
intersectPositionList [[xs]] = [xs]
intersectPositionList [xs,ys] = intersectPositionListHelp xs ys
--intersectPositionList [(xs:ys:zs)] = intersectPositionList
intersectPositionList (xs:ys:zs) = intersectPositionList ([(intersectPositionListHelp xs ys)] ++ zs) 
intersectPositionList _ = [[(404,400)]] 

{-
[[(2,6),(2,6)],[(7,7),(1,1)], [(8,8),(0,0)]],  
[[(7,7),(0,6)]]
           -}
--takes two list of postions and intersect them to one
intersectPositionListHelp :: [[(Int, Int)]] -> [[(Int, Int)]] -> [[(Int, Int)]]
intersectPositionListHelp [[]] _ = []
intersectPositionListHelp _ [[]] = []
intersectPositionListHelp [[x]] [[y]] = [intersectPositionListHelp2 [x] [y]] -- first argument 1 and second argument 1
intersectPositionListHelp [(x:xs)] [(y:ys)] = [intersectPositionListHelp2 (x:xs) (y:ys)] -- first argument 2 and second argument 2
intersectPositionListHelp (x:xs) [ys] = (intersectPositionListHelp [x] [ys]) ++  (intersectPositionListHelp xs [ys]) -- endlosschleife ?
intersectPositionListHelp (x:xs) (y:ys) = (intersectPositionListHelp (x:xs) [y]) ++ (intersectPositionListHelp (x:xs) ys)
intersectPositionListHelp _ _ = [[(404,404)]] 
 
 {-
[(7,7),(1,1)]  
[(7,7),(0,6)]
           -}
-- intersection between intervals
intersectPositionListHelp2 :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
intersectPositionListHelp2 [] _ = []
intersectPositionListHelp2 _ [] = []
intersectPositionListHelp2 [(x1,x2)] [(y1,y2)] | x1 <= y2 && x2 >= y1  = [(max x1 y1, min x2 y2)]
                                               | otherwise = []
intersectPositionListHelp2 (x:xs) (y:ys) = (intersectPositionListHelp2 [x] [y]) ++ (intersectPositionListHelp2 xs ys)                                                                           
                                                
--------------------------------------------------------------------------------
 
 
 
-- shuffle between factors
--------------------------------------------------------------------------------

regExp1 :: [[(String,Int)]] -> String -> [[[String]]]
regExp1 [] _ = []
regExp1 [s] c = [regExp s c]
regExp1 (x:xs) c = [(regExp x c)] ++ (regExp1 xs c)

--[("abb",0),("a",2),("baab",0)]
regExp :: [(String,Int)] -> String -> [[String]]
regExp [] _ = []
regExp [(s,0)] _ = [[s]]
regExp [(s,x)] c  =   [(shuffle42 s (helpNum c x))]
--regExp [(s,x)] c  =   [nub (shuffleWords s (helpNum c x))]
regExp (x:xs) c = (regExp [x] c) ++ (regExp xs c)

--
helpNum :: String -> Int -> String
helpNum s 1 = s
helpNum s x = s ++ (helpNum s (x-1))

--------------------------------------------------------------------------------

productWord1 :: [[[String]]] -> [String]
productWord1 [] = []
productWord1 [s] = productWord s
productWord1 (x:xs) = (productWord x) ++ productWord1 xs


-- gets a "regular expression" and returns all possible words
productWord :: [[String]] -> [String]
productWord [] = []
productWord [[s]] = [s]
productWord [s] = s
productWord [x,y] = productWordHelp x y
productWord (x:y:ys) = productWord ([(productWordHelp x y)] ++ ys)

productWordHelp :: [String] -> [String] -> [String]
productWordHelp [x] [y] = [x ++ y]
productWordHelp [x] (y:ys) = [x ++ y] ++ (productWordHelp [x] ys)
productWordHelp (x:xs) ys = (productWordHelp [x] ys) ++ (productWordHelp xs ys)
productWordHelp _ _ = ["error100"]

--------------------------------------------------------------------------------
---map reverse on [[(Int, Int)]] 

--returns a list where double occurences are counted
indexNumbers :: [[(Int, Int)]] -> [[(Int, Int, Int)]]
indexNumbers [] = []
indexNumbers [[(min1,max1)]] = [[(min1,max1,1)]]
indexNumbers [(x:xs)] = [indexNumbersHelp x xs 1]
indexNumbers (x:xs) = (indexNumbers [x]) ++ (indexNumbers xs)

indexNumbersHelp :: (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int, Int)]
indexNumbersHelp (min1,max1) [(min2,max2)] x | min1 == min2 && max1 == max2 = [(min1,max1, (x+1))]
                                             | otherwise = [(min1,max1,x), (min2,max2, 1)]
indexNumbersHelp (min1,max1) ((min2,max2):ys) x | min1 == min2 && max1 == max2 = (indexNumbersHelp (min1,max1) ys (x+1))
                                                | otherwise = [(min1,max1,x)] ++  (indexNumbersHelp (min2,max2) ys 1)                                              
indexNumbersHelp _ _ _ = [(404,404,606)]
--------------------------------------------------------------------------------


--compress the solution into a smaller result
compressionProductOfWordsForIndex1 :: [[(String,Int)]] -> [[(String,Int)]]
compressionProductOfWordsForIndex1 [] = []
compressionProductOfWordsForIndex1 [x] = [compressionProductOfWordsForIndex x]
compressionProductOfWordsForIndex1 (x:xs) = [compressionProductOfWordsForIndex x] ++ (compressionProductOfWordsForIndex1 xs) 


--compress the solution into a smaller result
compressionProductOfWordsForIndex :: [(String,Int)] -> [(String,Int)]
compressionProductOfWordsForIndex [] = []
compressionProductOfWordsForIndex [x] = [x]
compressionProductOfWordsForIndex [(x1,x2),(y1,y2)] | x2 == 0 && y2 == 0 = [((x1 ++ y1), x2)]
                                                    |otherwise = [(x1,x2),(y1,y2)]
compressionProductOfWordsForIndex ((x1,x2):((y1,y2): zs)) | (x2 == 0) && (y2 == 0) = compressionProductOfWordsForIndex (((x1 ++ y1), x2):zs)
                                                          |otherwise =  ((x1,x2): (compressionProductOfWordsForIndex ((y1,y2):zs)))


--------------------------------------------------------------------------------

-- return all possible words for a given word and a list of all possible next letter insertions
-- need as input the reverse list
productOfWordsForIndex :: String -> [[(Int, Int,Int)]] -> [[(String,Int)]]
productOfWordsForIndex _ [] = []
productOfWordsForIndex s [xs] = [productOfWordsForIndexHelp s xs 0 ]
productOfWordsForIndex s (x:xs) = [(productOfWordsForIndexHelp s x 0)] ++ (productOfWordsForIndex s xs )

-- gets the word, List of indexPositions, counter 0 and returns a list of String Int pairs while the String is the word where we wanna shuffle in the Int times a letter
productOfWordsForIndexHelp :: String -> [(Int, Int,Int)] -> Int -> [(String,Int)]
productOfWordsForIndexHelp [] [(0,0,x)] 0 = [("",x)] 
productOfWordsForIndexHelp [] [(min1,max1,x)] c | min1 == max1 && max1 == c = [("",x)]
                                                | otherwise = [("wrong input String is empty",404)]
productOfWordsForIndexHelp s [(0,0,x)] 0 = [("",x)] ++ [(s, 0)]                      
productOfWordsForIndexHelp s [(min1,max1,x)] 0 | min1 == 0 && max1 > 0 = [("",0)] ++ [((helpMax s 1 max1), x)] ++ [((helpMax2 s 1 max1), 0)] 
                                               | min1 > 0 = productOfWordsForIndexHelp s [(min1,max1,x)] 1
                                               | otherwise = [("wrong input position is to low",404)]
-- from here on c > 0 and max1 > 0, min1 > 0 for PosList length 1
productOfWordsForIndexHelp [s] [(min1,max1,x)] c | min1 == max1 && max1 == c = [([s], 0)] ++ [("",x)]
                                                 | otherwise = [("wrong input String is empty and pos are greater",404)]
--sting length > 1 
productOfWordsForIndexHelp (s:xs) [(min1,max1,x)] c | min1 == max1 && max1 == c = [([s], 0)] ++ [("",x)] ++ [(xs,0)]
                                                    | (min1 == max1) && (max1 > c) = [((s:(helpMax xs (c+1) max1)),0)] ++ [("",x)] ++ [((helpMax2 xs (c+1) max1),0)]  --[([s], 0)] ++  war am Anfang
                                                    | min1 == c && max1 > c = [([s], 0)] ++ [((helpMax xs (c+1) max1 ),x)] ++ [((helpMax2 xs (c+1) max1) ,0)]
                                                    | otherwise = [([s],0)] ++ productOfWordsForIndexHelp xs [(min1,max1,x)] (c+1) -- the case where min1 > c
-- from here on the PosList ist longer then 1
productOfWordsForIndexHelp (s1:s1s) ((min1,max1,x):xs) c | min1 == 0 && max1 == 0 && c == 0 = [("",x)] ++ (productOfWordsForIndexHelp (s1:s1s) xs 1)
                                                         | min1 == 0 && max1 > 0 = [((helpMax (s1:s1s) (c+1) max1),x)] ++ (productOfWordsForIndexHelp (helpMax2 (s1:s1s) (c+1) max1) xs (max1 +1))
                                                         | min1 > 0 && c== 0 = productOfWordsForIndexHelp (s1:s1s) ((min1,max1,x):xs) 1
                                                         | min1 == max1 && max1 == c = [([s1], 0)] ++ [("",x)] ++ (productOfWordsForIndexHelp s1s xs (c+1))
                                                         | min1 > c = [([s1], 0)] ++ (productOfWordsForIndexHelp s1s ((min1,max1,x):xs) (c+1))
                                                         | otherwise = [([s1], 0)] ++ [((helpMax s1s (c+1) max1),x)] ++ (productOfWordsForIndexHelp (helpMax2 s1s (c+1) max1) xs (max1+1))  -- min1 = c and max1 > c
productOfWordsForIndexHelp _ _ _ = [("This input shouldn't be possible",808)] 


-- returns the string where we wanna insert the letter
helpMax :: String -> Int -> Int -> String
helpMax [] _ _ = []
helpMax [x] c max1 | c <= max1 = [x]
                   |otherwise = []
helpMax (x:xs) c max1 | c <= max1 = (x: (helpMax xs (c+1) max1))
                      |otherwise = [] 
                     
-- returns the string where we wanna insert the next letters                     
helpMax2 :: String -> Int -> Int -> String
helpMax2 [] _ _ = []
helpMax2 [x] c max1 | c <= max1 = []
                    |otherwise = [x]
helpMax2 (x:xs) c max1 | c <= max1 = helpMax2 xs (c+1) max1 
                       |otherwise = (x:xs) 

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

--abba cc
--shuffle a word to another word and delete double occurences
--first argument given word and 2nd the word/letter to shuffle in
shuffleWords :: String -> String -> [String]
shuffleWords [] [] = []
shuffleWords [] x = [x]
shuffleWords s1 [l] = [[l] ++ s1] ++ shuffleLetterToWord s1 [] l
shuffleWords s1 (s2:s2s) = nub((shuffleWords ([s2]++s1) s2s) ++ (shuffleWordsHelp [s1] (s2:s2s)))
shuffleWords _ _ = ["wrong input shuffleWords"] 

-- abba c gives out all possible shuffles of a word and a letter
shuffleWordsHelp :: [String] -> String -> [String]
shuffleWordsHelp [s1] [s2] = shuffleLetterToWord s1 [] s2
shuffleWordsHelp [s1] (s2:s2s) = shuffleWordsHelp (shuffleLetterToWord s1 [] s2) s2s
shuffleWordsHelp (s1:s1s) [s2] = (shuffleLetterToWord s1 [] s2) ++ (shuffleWordsHelp s1s [s2])
shuffleWordsHelp (s1:s1s) s2s = (shuffleWordsHelp [s1] s2s) ++ (shuffleWordsHelp s1s s2s)
shuffleWordsHelp _ _ = ["wrong input shuffleWordsHelp"]

--take a string and a char and give a list out which insert the char at each psosition
shuffleLetterToWord :: [Char] -> String -> Char -> [String]
shuffleLetterToWord [] [] l = [[l]]
shuffleLetterToWord [s] [] l | s == l = [[s] ++ [l]]
                             | otherwise = [[s] ++ [l], [l] ++ [s]]
shuffleLetterToWord [s] xs l | s == l = []
                             | otherwise = [xs ++ [s] ++ [l]]  
shuffleLetterToWord (s1:s1s) xs l | s1 == l = shuffleLetterToWord s1s (xs ++ [s1]) l 
                                  | otherwise = [xs ++ [s1] ++ [l] ++ s1s] ++ (shuffleLetterToWord s1s (xs ++ [s1]) l )
shuffleLetterToWord _ _ _ = ["Wrong input in shuffleLetterToWord"]


--------------------------------------------------------------------------------


-- get for c and given a, b, ab, bc, abc possible Partitions reduced with our table
-- partitions for bc and abc needed
-- string with table needed.
-- column (b , ab) of string with table is needed 
partitionIndexReduction :: Int -> Int -> Int -> Int -> Int -> (String, [[Int]]) -> (String, [[Int]],[Partition], [Partition])
partitionIndexReduction c b bc ab abc (s,xs) = (s,xs, (helpReduction (partitionFillUp (partitionMaxElemMaxLength bc b c) c) (getColumnIndexTableHelp 2 (s,xs))), (helpReduction (partitionFillUp (partitionMaxElemMaxLength abc ab c) c)) (getColumnIndexTableHelp 3 (s,xs)))  


-- fills up 0 to all Partitions with less elements then the given c
partitionFillUp :: [Partition] -> Int -> [Partition] 
partitionFillUp [] _ = []
partitionFillUp [Partition x] n = [Partition (fillUp x n)] where
                                               fillUp :: [Int]-> Int -> [Int]
                                               fillUp xs n1 | (length xs) == n1 = xs
                                                            | otherwise = fillUp (xs ++ [0]) n1
partitionFillUp (x:xs) n = ((partitionFillUp [x] n) ++ (partitionFillUp xs n))                                                              



-- delete a Partition if a value isn't in the table
helpReduction :: [Partition] -> [Int] -> [Partition]
helpReduction [] _ = []
helpReduction [Partition x] xs = reduction2 x xs x where
                                  reduction2 :: [Int] -> [Int] -> [Int] -> [Partition]
                                  reduction2 [] _ _ = []
                                  reduction2 [y] zs xs' | (elem y zs) == False = []
                                                        | otherwise = [Partition xs']
                                  reduction2 (y:ys) zs xs' | (elem y zs) == False = []
                                                           | otherwise = reduction2 ys zs xs'
helpReduction ((Partition x):xxs) xs = (reduction2 x xs x) ++ (helpReduction xxs xs) where
                                      reduction2 :: [Int] -> [Int] -> [Int] -> [Partition]
                                      reduction2 [] _ _ = []
                                      reduction2 [y] zs xs' | (elem y zs) == False = []
                                                            | otherwise = [Partition xs']
                                      reduction2 (y:ys) zs xs' | (elem y zs) == False = []
                                                               | otherwise = reduction2 ys zs xs'

--------------------------------------------------------------------------------
-- calculates for a column of the indexTable where their indexes align
indexPositions :: [Int] -> [(Int, Int, Int)]
indexPositions [] = [(100,100,103)]
indexPositions [x] = [(x, 1, 1)]
indexPositions (x:xs) = indexPositionsHelp (x:xs) 0 0 where
                         indexPositionsHelp :: [Int] -> Int -> Int -> [(Int, Int, Int)]
                         indexPositionsHelp [] _ _ = []
                         indexPositionsHelp [y] _ xc = [(y, xc, xc)]  
                         indexPositionsHelp (y:ys) vc xc | vc == y = [(y, xc, (last2 ys vc (xc)))] ++ (indexPositionsHelp (last1 ys vc) (vc+1) (last2 ys vc (xc+1)))
                         indexPositionsHelp (y:ys) vc xc | vc < y = indexPositionsHelp (y:ys) (vc+1) xc  
                         indexPositionsHelp _ _ _ = [(100,100,101)]  



last1 :: [Int] -> Int -> [Int]
last1 [y] z | y==z = []
            | otherwise = [y]
last1 (y:ys) z | y==z = last1 ys z
               | otherwise = (y:ys)
last1 _ _ = [100]

last2 :: [Int] -> Int -> Int ->  Int
last2 [y] z yc | y==z = (yc+1)
last2 [_] _ yc = yc
last2 (y:ys) z yc | y==z = (last2 ys z (yc+1))
last2 (_:_) _ yc = yc   
last2 _ _ yc = yc               
--------------------------------------------------------------------------------


-- convert all Partitions to a list of possible Positions
partitionToIndexPositions :: String -> Int -> Int -> Int -> Int -> Int -> [[[(Int,Int)]]] 
partitionToIndexPositions s c b bc ab abc = [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength bc b c) c) (getColumnIndexTableHelp 2 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 2 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength abc ab c) c) (getColumnIndexTableHelp 3 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 3 (indexTableWord s)))]


indexPosition1 :: [Partition] -> [(Int,Int,Int)] -> [[(Int,Int)]]
indexPosition1 [] _ = []
indexPosition1 [Partition x] ys = [indexPosition2 x (reverse ys)]
indexPosition1 ((Partition x) :xs) ys = [(indexPosition2 x (reverse ys))] ++ (indexPosition1 xs ys) 

indexPosition2 :: [Int] -> [(Int,Int,Int)] -> [(Int,Int)] 
indexPosition2 [] _ = []
indexPosition2 [y] [(z,z1,z2)] | y == z = [(z1,z2)]
                               | otherwise = [(100,107)]
indexPosition2 [y] ((z,z1,z2): zs) | y == z = [(z1,z2)] 
                                   | otherwise = indexPosition2 [y] zs
indexPosition2 (y:ys) ((z,z1,z2): zs) | y == z = [(z1,z2)] ++ indexPosition2 ys ((z,z1,z2): zs)
                                      | otherwise = indexPosition2 (y:ys) zs
indexPosition2 _ _ = [(100,109)]

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------

--makes from partitionToString a single String
partitionToStringConcat :: Partition -> Int -> Int -> String
partitionToStringConcat x y z = concat (partitionToString x y z) 

--partition in binary string a and b
--convert a given Partition in a string with given number of a's and b's 
partitionToString :: Partition -> Int -> Int -> [String]              
partitionToString (Partition []) 0 0 = [ ]
partitionToString (Partition []) x 0 = ["a"] ++ partitionToString (Partition []) (x-1) 0
partitionToString (Partition []) x y = ["b"] ++ partitionToString (Partition []) x (y-1)                 
partitionToString (Partition [0]) 1 1 = ["b", "a"]
partitionToString (Partition [0]) 1 0 = ["a"]
partitionToString (Partition [0]) 0 1 = ["b"]
partitionToString (Partition [0]) y 0 = ["a"] ++ partitionToString (Partition [0]) (y-1) 0
partitionToString (Partition [0]) y x = ["b"] ++ partitionToString (Partition [0]) y (x-1)
partitionToString (Partition [1]) 1 1 = ["a", "b"]
partitionToString (Partition [z]) y x | x > 1 = ["b"] ++ partitionToString (Partition [z]) y (x-1)
partitionToString (Partition [z]) y 1 | y > 0 = ["a"] ++ partitionToString (Partition [z-1]) (y-1) 1
partitionToString (Partition (z:zs)) y x = partitionToString' (Partition (reverse(z:zs))) y x (length (z:zs)) 0
             where partitionToString' :: Partition -> Int -> Int -> Int -> Int -> [String]
                    -- number of a's equal to zero, so all letters set
                   partitionToString' (Partition []) y' x' l' a' | y' == 0 = []
                  -- partitionToString' (Partition []) 0 0 _ _ = []
                  -- partitionToString' (Partition []) y' 0 l' a'  
                    -- number of b's equal to zero, so we are done or we need to place the rest a's
                                                                 | x' == 0 = ["a"] ++ partitionToString' (Partition []) (y'-1) 0 l' (a'+1) 
--                   partitionToString' (Partition []) _ _ _ _ =
                                                                 | otherwise = ["wrong input empty elem"]
                      -- length is shorter then number of b's we have to fill the front with b's of the difference
                   partitionToString' (Partition [z']) y' x' l' a' | l' < x' = ["b"] ++ partitionToString' (Partition [z']) y' (x'-1) l' a'
                      -- the numbers of set a's is smaller then the number in the partition, so we need more a's before the b
                                                                   | a' < z' = ["a"] ++ partitionToString' (Partition [z']) (y'-1) x' l' (a'+1)
                     --we have the same number of a's set and Partition value so we need now the b                                              
                                                                   | a' == z' = ["b"] ++ partitionToString' (Partition ([])) y' (x'-1) (l'-1) a'                 
                                                                   | otherwise = ["wrong input one elem"]
                    -- partition length smaller the number of b's so we need b's at the beginning                                               
                   partitionToString' (Partition (z':zs')) y' x' l' a' | l' < x' = ["b"] ++ partitionToString' (Partition (z':zs')) y' (x'-1) l' a'
                     -- partition value is smaller then number of a's we need to set more a's
                                                                       | a' < z' = ["a"] ++ partitionToString' (Partition (z':zs')) (y'-1) x' l' (a'+1)
                    --the partition number is equal to the set a's so we need a b                                                   
                                                                       | a' == z' = ["b"] ++ partitionToString' (Partition (zs')) y' (x'-1) (l'-1) a'
                                                                       | otherwise = ["wrong input longer list"]
                                                                
--Takes a list of Partitions and convert them into binary strings (the number of a's and b's must be given                                                                
partitionListToString :: [Partition] -> Int -> Int -> [String]
partitionListToString [] _ _ = []
partitionListToString [Partition []] 0 0 = [""]
partitionListToString [x] a b = [partitionToStringConcat x a b]
partitionListToString (x:xs) a b = ((partitionToStringConcat x a b): partitionListToString xs a b)
--------------------------------------------------------------------------------             
             



--------------------------------------------------------------------------------
--for alphabet size all values letters to the highest (max 10) is needed
parikhMatrixElemFromWord :: String -> [Int]            
parikhMatrixElemFromWord s = parikhMatrixValues s (alphabetLengthAlphabetical s)

--calculate highest letter in a word alphabetical order
alphabetLengthAlphabetical :: String -> Int
alphabetLengthAlphabetical s | elem 'j' s = 10
                             | elem 'i' s = 9
                             | elem 'h' s = 8
                             | elem 'g' s = 7
                             | elem 'f' s = 6
                             | elem 'e' s = 5
                             | elem 'd' s = 4
                             | elem 'c' s = 3
                             | elem 'b' s = 2
                             | elem 'a' s = 1
                             | otherwise = 0

--not used anymore     
--calculate the alphabet size of a word             
alphabetLength :: String -> Int
alphabetLength z = alphabetLength' z 0 [] where 
                                 alphabetLength' [] x _ = x
                                 alphabetLength' [x] y xs | elem x xs == False = y+1
                                                          | otherwise = y
                                 alphabetLength' (x:xs) y ys | elem x ys == False = alphabetLength' xs (y+1) (ys ++ [x])             
                                                             | otherwise = alphabetLength' xs y ys             
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- converting a list of Parikh Matrix values into a Parikh Matrix having a list of columns 
-- only for Parikh Matrices of length 10            
--ParikhMatrix for a word list of columns         
parikhMatrixFromWord :: String -> [[Int]]             
parikhMatrixFromWord s = parikhMatrixFromWord' s ((alphabetLength s)+1) (parikhMatrixValues s (alphabetLength s)) where
                            parikhMatrixFromWord' :: String -> Int -> [Int] -> [[Int]]
                            parikhMatrixFromWord' _ l xs | l == 0 || l == 1 = [[0]]
                                                         | l == 2 = [[1, 0], [(length xs),1]]
                                                         | l == 3 = [[1,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:ys) i | i == 1 = [[y0, 1, 0]] ++ (pMFromWord (y1:ys) 2)
                                                                                                        | i == 2 = [[y1, y0, 1]]
                                                                                                        | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]]                        
                            parikhMatrixFromWord' _ l xs | l == 4 = [[1,0,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:y2:ys) i | i == 1 = [[y0, 1, 0, 0]] ++ (pMFromWord (y1:y2:ys) 2)
                                                                                                           | i == 2 = [[y1, y0, 1, 0]] ++ (pMFromWord (y2:ys) 3)
                                                                                                           | i == 3 = [[y2, y1, y0,1]]
                                                                                                           | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]]                           
                            parikhMatrixFromWord' _ l xs | l == 5 = [[1,0,0,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:y2:y3:ys) i | i == 1 = [[y0, 1, 0, 0,0]] ++ (pMFromWord (y1:y2:y3:ys) 2)
                                                                                                              | i == 2 = [[y1, y0, 1, 0,0]] ++ (pMFromWord (y2:y3:ys) 3)
                                                                                                              | i == 3 = [[y2, y1, y0,1, 0]] ++ (pMFromWord (y3:ys) 4)
                                                                                                              | i == 4 = [[y3, y2, y1, y0,1]]
                                                                                                              | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]]                
                            parikhMatrixFromWord' _ l xs | l == 6 = [[1,0,0,0,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:y2:y3:y4:ys) i | i == 1 = [[y0, 1, 0, 0,0,0]] ++ (pMFromWord (y1:y2:y3:y4:ys) 2)
                                                                                                                 | i == 2 = [[y1, y0, 1, 0,0,0]] ++ (pMFromWord (y2:y3:y4:ys) 3)
                                                                                                                 | i == 3 = [[y2, y1, y0,1, 0,0]] ++ (pMFromWord (y3:y4:ys) 4)
                                                                                                                 | i == 4 = [[y3, y2, y1, y0,1,0]] ++ (pMFromWord (y4:ys) 5)
                                                                                                                 | i == 5 = [[y4,y3, y2, y1, y0,1]]
                                                                                                                 | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]]   
                            parikhMatrixFromWord' _ l xs | l == 7 = [[1,0,0,0,0,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:y2:y3:y4:y5:ys) i | i == 1 = [[y0, 1, 0, 0,0,0,0]] ++ (pMFromWord (y1:y2:y3:y4:y5:ys) 2)
                                                                                                                    | i == 2 = [[y1, y0, 1, 0,0,0,0]] ++ (pMFromWord (y2:y3:y4:y5:ys) 3)
                                                                                                                    | i == 3 = [[y2, y1, y0,1, 0,0,0]] ++ (pMFromWord (y3:y4:y5:ys) 4)
                                                                                                                    | i == 4 = [[y3, y2, y1, y0,1,0,0]] ++ (pMFromWord (y4:y5:ys) 5)
                                                                                                                    | i == 5 = [[y4,y3, y2, y1, y0,1,0]] ++ (pMFromWord (y5:ys) 6)
                                                                                                                    | i == 6 = [[y5,y4,y3, y2, y1, y0,1]]
                                                                                                                    | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]]  
                            parikhMatrixFromWord' _ l xs | l == 8 = [[1,0,0,0,0,0,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:y2:y3:y4:y5:y6:ys) i | i == 1 = [[y0, 1, 0, 0,0,0,0,0]] ++ (pMFromWord (y1:y2:y3:y4:y5:y6:ys) 2)
                                                                                                                       | i == 2 = [[y1, y0, 1, 0,0,0,0,0]] ++ (pMFromWord (y2:y3:y4:y5:y6:ys) 3)
                                                                                                                       | i == 3 = [[y2, y1, y0,1, 0,0,0,0]] ++ (pMFromWord (y3:y4:y5:y6:ys) 4)
                                                                                                                       | i == 4 = [[y3, y2, y1, y0,1,0,0,0]] ++ (pMFromWord (y4:y5:y6:ys) 5)
                                                                                                                       | i == 5 = [[y4,y3, y2, y1, y0,1,0,0]] ++ (pMFromWord (y5:y6:ys) 6)
                                                                                                                       | i == 6 = [[y5,y4,y3, y2, y1, y0,1,0]] ++ (pMFromWord (y6:ys) 7)
                                                                                                                       | i == 7 = [[y6,y5,y4,y3, y2, y1, y0,1]]
                                                                                                                       | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]] 
                            parikhMatrixFromWord' _ l xs | l == 9 = [[1,0,0,0,0,0,0,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:y2:y3:y4:y5:y6:y7:ys) i | i == 1 = [[y0, 1, 0, 0,0,0,0,0,0]] ++ (pMFromWord (y1:y2:y3:y4:y5:y6:y7:ys) 2)
                                                                                                                          | i == 2 = [[y1, y0, 1, 0,0,0,0,0,0]] ++ (pMFromWord (y2:y3:y4:y5:y6:y7:ys) 3)
                                                                                                                          | i == 3 = [[y2, y1, y0,1, 0,0,0,0,0]] ++ (pMFromWord (y3:y4:y5:y6:y7:ys) 4)
                                                                                                                          | i == 4 = [[y3, y2, y1, y0,1,0,0,0,0]] ++ (pMFromWord (y4:y5:y6:y7:ys) 5)  
                                                                                                                          | i == 5 = [[y4,y3, y2, y1, y0,1,0,0,0]] ++ (pMFromWord (y5:y6:y7:ys) 6)
                                                                                                                          | i == 6 = [[y5,y4,y3, y2, y1, y0,1,0,0]] ++ (pMFromWord (y6:y7:ys) 7)
                                                                                                                          | i == 7 = [[y6,y5,y4,y3, y2, y1, y0,1,0]] ++ (pMFromWord (y7:ys) 8)
                                                                                                                          | i == 8 = [[y7,y6,y5,y4,y3, y2, y1, y0,1]]
                                                                                                                          | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]]                                                     
                            parikhMatrixFromWord' _ l xs | l == 10 = [[1,0,0,0,0,0,0,0,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:y2:y3:y4:y5:y6:y7:y8:ys) i | i == 1 = [[y0, 1, 0, 0,0,0,0,0,0,0]] ++ (pMFromWord (y1:y2:y3:y4:y5:y6:y7:y8:ys) 2)
                                                                                                                             | i == 2 = [[y1, y0, 1, 0,0,0,0,0,0,0]] ++ (pMFromWord (y2:y3:y4:y5:y6:y7:y8:ys) 3)
                                                                                                                             | i == 3 = [[y2, y1, y0,1, 0,0,0,0,0,0]] ++ (pMFromWord (y3:y4:y5:y6:y7:y8:ys) 4)
                                                                                                                             | i == 4 = [[y3, y2, y1, y0,1,0,0,0,0,0]] ++ (pMFromWord (y4:y5:y6:y7:y8:ys) 5)  
                                                                                                                             | i == 5 = [[y4,y3, y2, y1, y0,1,0,0,0,0]] ++ (pMFromWord (y5:y6:y7:y8:ys) 6)
                                                                                                                             | i == 6 = [[y5,y4,y3, y2, y1, y0,1,0,0,0]] ++ (pMFromWord (y6:y7:y8:ys) 7)
                                                                                                                             | i == 7 = [[y6,y5,y4,y3, y2, y1, y0,1,0,0]] ++ (pMFromWord (y7:y8:ys) 8)
                                                                                                                             | i == 8 = [[y7,y6,y5,y4,y3, y2, y1, y0,1,0]] ++ (pMFromWord (y8:ys) 9)
                                                                                                                             | i == 9 = [[y8,y7,y6,y5,y4,y3, y2, y1, y0,1]]
                                                                                                                             | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]]                                                                                                                                                                    
                            parikhMatrixFromWord' _ l xs | l == 11 = [[1,0,0,0,0,0,0,0,0,0,0]] ++ (pMFromWord xs 1) where
                                                                                pMFromWord :: [Int] -> Int -> [[Int]]
                                                                                pMFromWord (y0:y1:y2:y3:y4:y5:y6:y7:y8:y9:ys) i | i == 1 = [[y0, 1, 0, 0,0,0,0,0,0,0,0]] ++ (pMFromWord (y1:y2:y3:y4:y5:y6:y7:y8:y9:ys) 2)
                                                                                                                                | i == 2 = [[y1, y0, 1, 0,0,0,0,0,0,0,0]] ++ (pMFromWord (y2:y3:y4:y5:y6:y7:y8:y9:ys) 3)
                                                                                                                                | i == 3 = [[y2, y1, y0,1, 0,0,0,0,0,0,0]] ++ (pMFromWord (y3:y4:y5:y6:y7:y8:y9:ys) 4)
                                                                                                                                | i == 4 = [[y3, y2, y1, y0,1,0,0,0,0,0,0]] ++ (pMFromWord (y4:y5:y6:y7:y8:y9:ys) 5)  
                                                                                                                                | i == 5 = [[y4,y3, y2, y1, y0,1,0,0,0,0,0]] ++ (pMFromWord (y5:y6:y7:y8:y9:ys) 6)
                                                                                                                                | i == 6 = [[y5,y4,y3, y2, y1, y0,1,0,0,0,0]] ++ (pMFromWord (y6:y7:y8:y9:ys) 7)
                                                                                                                                | i == 7 = [[y6,y5,y4,y3, y2, y1, y0,1,0,0,0]] ++ (pMFromWord (y7:y8:y9:ys) 8)
                                                                                                                                | i == 8 = [[y7,y6,y5,y4,y3, y2, y1, y0,1,0,0]] ++ (pMFromWord (y8:y9:ys) 9)
                                                                                                                                | i == 9 = [[y8,y7,y6,y5,y4,y3, y2, y1, y0,1,0]] ++ (pMFromWord (y9:ys) 10)
                                                                                                                                | i == 10 = [[y9,y8,y7,y6,y5,y4,y3, y2, y1, y0,1]]
                                                                                                                                | otherwise = [[0]]
                                                                                pMFromWord _ _ = [[0]] 
                            parikhMatrixFromWord' _ _ _ |otherwise = [[0]]
--------------------------------------------------------------------------------
--end of Parikh Matrix




             
--------------------------------------------------------------------------------             
--ParikMatrixValues for a given alphabet length maximal 10
parikhMatrixValues :: String -> Int -> [Int]
parikhMatrixValues s x | x == 0 = [0]
                       | x == 1 = [(length s)]
                       | x == 2 = parikhMatrixValB s [0, 0, 0 ] where
                                    parikhMatrixValB :: String -> [Int] -> [Int]
                                    parikhMatrixValB [] y = y
                                    parikhMatrixValB [y] [a,b,ab] | y == 'a' = [a+1, b, ab]
                                                                  | y == 'b' = [a, b+1, ab+a]
                                                                  | otherwise = [0]
                                    parikhMatrixValB (y:ys) [a,b,ab] | y == 'a' = parikhMatrixValB ys [a+1, b, ab]
                                                                     | y == 'b' = parikhMatrixValB ys [a, b+1, ab+a]
                                                                     | otherwise = [0]
                                    parikhMatrixValB _ _ = [0]                                 
parikhMatrixValues s x | x == 3 = parikhMatrixValC s [0, 0, 0, 0, 0, 0 ] where
                                    parikhMatrixValC :: String -> [Int] -> [Int]
                                    parikhMatrixValC [] y = y
                                    parikhMatrixValC [y] [a,b,ab, c, bc, abc] | y == 'a' = [a+1, b, ab, c, bc, abc]
                                                                              | y == 'b' = [a, b+1, ab+a, c, bc, abc]
                                                                              | y == 'c' = [a, b, ab, c+1, bc+b, abc+ab]
                                                                              | otherwise = [0]
                                    parikhMatrixValC (y:ys) [a,b,ab,c,bc,abc] | y == 'a' = parikhMatrixValC ys [a+1, b, ab, c, bc, abc]
                                                                              | y == 'b' = parikhMatrixValC ys [a, b+1, ab+a, c, bc, abc]
                                                                              | y == 'c' = parikhMatrixValC ys [a, b, ab, c+1, bc+b, abc+ab]
                                                                              | otherwise = [0]
                                    parikhMatrixValC _ _ = [0]                                          
parikhMatrixValues s x | x == 4 = parikhMatrixValD s [0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] where
                                    parikhMatrixValD :: String -> [Int] -> [Int]
                                    parikhMatrixValD [] y = y
                                    parikhMatrixValD [y] [a,b,ab, c, bc, abc, d, cd, bcd, abcd] | y == 'a' = [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd]
                                                                                                | y == 'b' = [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd]
                                                                                                | y == 'c' = [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd]
                                                                                                | y == 'd' = [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc] 
                                                                                                | otherwise = [0]
                                    parikhMatrixValD (y:ys) [a,b,ab,c,bc,abc, d, cd, bcd, abcd] | y == 'a' = parikhMatrixValD ys [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd]
                                                                                                | y == 'b' = parikhMatrixValD ys [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd]
                                                                                                | y == 'c' = parikhMatrixValD ys [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd]
                                                                                                | y == 'd' = parikhMatrixValD ys [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc] 
                                                                                                | otherwise = [0]
                                    parikhMatrixValD _ _ = [0]                                                             
parikhMatrixValues s x | x == 5 = parikhMatrixValE s [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] where
                                    parikhMatrixValE :: String -> [Int] -> [Int]
                                    parikhMatrixValE [] y = y
                                    parikhMatrixValE [y] [a,b,ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde] | y == 'a' = [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde]
                                                                                                | y == 'b' = [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde]
                                                                                                | y == 'c' = [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde]
                                                                                                | y == 'd' = [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde] 
                                                                                                | y == 'e' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd] 
                                                                                                | otherwise = [0]
                                    parikhMatrixValE (y:ys) [a,b,ab,c,bc,abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde] | y == 'a' = parikhMatrixValE ys [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde]
                                                                                                                         | y == 'b' = parikhMatrixValE ys [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde]
                                                                                                                         | y == 'c' = parikhMatrixValE ys [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde]
                                                                                                                         | y == 'd' = parikhMatrixValE ys [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde] 
                                                                                                                         | y == 'e' = parikhMatrixValE ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd] 
                                                                                                                         | otherwise = [0]
                                    parikhMatrixValE _ _ = [0] 
parikhMatrixValues s x | x == 6 = parikhMatrixValF s [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] where
                                    parikhMatrixValF :: String -> [Int] -> [Int]
                                    parikhMatrixValF [] y = y
                                    parikhMatrixValF [y] [a,b,ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef] | y == 'a' = [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]
                                              | y == 'b' = [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]
                                              | y == 'c' = [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]
                                              | y == 'd' = [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef] 
                                              | y == 'e' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef] 
                                              | y == 'f' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde] 
                                              | otherwise = [0]
                                    parikhMatrixValF (y:ys) [a,b,ab,c,bc,abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef] | y == 'a' = parikhMatrixValF ys [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]
                                              | y == 'b' = parikhMatrixValF ys [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]
                                              | y == 'c' = parikhMatrixValF ys [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]
                                              | y == 'd' = parikhMatrixValF ys [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef] 
                                              | y == 'e' = parikhMatrixValF ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef] 
                                              | y == 'f' = parikhMatrixValF ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde] 
                                              | otherwise = [0]
                                    parikhMatrixValF _ _ = [0]          
parikhMatrixValues s x | x == 7 = parikhMatrixValG s [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] where
                                    parikhMatrixValG :: String -> [Int] -> [Int]
                                    parikhMatrixValG [] y = y
                                    parikhMatrixValG [y] [a,b,ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg] | y == 'a' = [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg]
                                       | y == 'b' = [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg]
                                       | y == 'c' = [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg]
                                       | y == 'd' = [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg] 
                                       | y == 'e' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg] 
                                       | y == 'f' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde, g, fg, efg, defg, cdefg, bcdefg, abcdefg] 
                                       | y == 'g' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g+1, fg+f, efg+ef, defg+def, cdefg+cdef, bcdefg+bcdef, abcdefg+abcdef]
                                       | otherwise = [0]
                                    parikhMatrixValG (y:ys) [a,b,ab,c,bc,abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg] | y == 'a' = parikhMatrixValG ys [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg]
                                       | y == 'b' = parikhMatrixValG ys [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg]
                                       | y == 'c' = parikhMatrixValG ys [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg]
                                       | y == 'd' = parikhMatrixValG ys [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg] 
                                       | y == 'e' = parikhMatrixValG ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg] 
                                       | y == 'f' = parikhMatrixValG ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde, g, fg, efg, defg, cdefg, bcdefg, abcdefg] 
                                       | y == 'g' = parikhMatrixValG ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g+1, fg+f, efg+ef, defg+def, cdefg+cdef, bcdefg+bcdef, abcdefg+abcdef] 
                                       | otherwise = [0]
                                    parikhMatrixValG _ _ = [0]                                                        
parikhMatrixValues s x | x == 8 = parikhMatrixValH s [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] where
                                    parikhMatrixValH :: String -> [Int] -> [Int]
                                    parikhMatrixValH [] y = y
                                    parikhMatrixValH [y] [a,b,ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] | y == 'a' = [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]
                                                                                           | y == 'b' = [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]
                                                                                           | y == 'c' = [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]
                                                                                           | y == 'd' = [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] 
                                                                                           | y == 'e' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] 
                                                                                           | y == 'f' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] 
                                                                                           | y == 'g' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g+1, fg+f, efg+ef, defg+def, cdefg+cdef, bcdefg+bcdef, abcdefg+abcdef, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]
                                                                                           | y == 'h' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h+1, gh+g, fgh+fg, efgh+efg, defgh+defg, cdefgh+cdefg, bcdefgh+bcdefg, abcdefgh+abcdefg]
                                                                                           | otherwise = [0]
                                    parikhMatrixValH (y:ys) [a,b,ab,c,bc,abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] | y == 'a' = parikhMatrixValH ys [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]
                                                                                           | y == 'b' = parikhMatrixValH ys [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]
                                                                                           | y == 'c' = parikhMatrixValH ys [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]
                                                                                           | y == 'd' = parikhMatrixValH ys [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] 
                                                                                           | y == 'e' = parikhMatrixValH ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] 
                                                                                           | y == 'f' = parikhMatrixValH ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] 
                                                                                           | y == 'g' = parikhMatrixValH ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g+1, fg+f, efg+ef, defg+def, cdefg+cdef, bcdefg+bcdef, abcdefg+abcdef, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] 
                                                                                           | y == 'h' = parikhMatrixValH ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h+1, gh+g, fgh+fg, efgh+efg, defgh+defg, cdefgh+cdefg, bcdefgh+bcdefg, abcdefgh+abcdefg] 
                                                                                           | otherwise = [0] 
                                    parikhMatrixValH _ _ = [0]    
parikhMatrixValues s x | x == 9 = parikhMatrixValI s [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] where
                                    parikhMatrixValI :: String -> [Int] -> [Int]
                                    parikhMatrixValI [] y = y
                                    parikhMatrixValI [y] [a,b,ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] | y == 'a' = [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                                                                                           | y == 'b' = [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                                                                                           | y == 'c' = [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                                                                                           | y == 'd' = [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] 
                                                                                           | y == 'e' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] 
                                                                                           | y == 'f' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] 
                                                                                           | y == 'g' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g+1, fg+f, efg+ef, defg+def, cdefg+cdef, bcdefg+bcdef, abcdefg+abcdef, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                                                                                           | y == 'h' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h+1, gh+g, fgh+fg, efgh+efg, defgh+defg, cdefgh+cdefg, bcdefgh+bcdefg, abcdefgh+abcdefg, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                                                                                           | y == 'i' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i+1, hi+h, ghi+gh, fghi+fgh, efghi+efgh, defghi+defgh, cdefghi+cdefgh, bcdefghi+bcdefgh, abcdefghi+abcdefgh] 
                                                                                           | otherwise = [0]
                                    parikhMatrixValI (y:ys) [a,b,ab,c,bc,abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] | y == 'a' = parikhMatrixValI ys [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                                                                                           | y == 'b' = parikhMatrixValI ys [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                                                                                           | y == 'c' = parikhMatrixValI ys [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                                                                                           | y == 'd' = parikhMatrixValI ys [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] 
                                                                                           | y == 'e' = parikhMatrixValI ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] 
                                                                                           | y == 'f' = parikhMatrixValI ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] 
                                                                                           | y == 'g' = parikhMatrixValI ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g+1, fg+f, efg+ef, defg+def, cdefg+cdef, bcdefg+bcdef, abcdefg+abcdef, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] 
                                                                                           | y == 'h' = parikhMatrixValI ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h+1, gh+g, fgh+fg, efgh+efg, defgh+defg, cdefgh+cdefg, bcdefgh+bcdefg, abcdefgh+abcdefg, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] 
                                                                                           | y == 'i' = parikhMatrixValI ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i+1, hi+h, ghi+gh, fghi+fgh, efghi+efgh, defghi+defgh, cdefghi+cdefgh, bcdefghi+bcdefgh, abcdefghi+abcdefgh] 
                                                                                           | otherwise = [0]
                                    parikhMatrixValI _ _ = [0]                                                        
parikhMatrixValues s x | x == 10 = parikhMatrixValJ s [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] where
                                    parikhMatrixValJ :: String -> [Int] -> [Int]
                                    parikhMatrixValJ [] y = y
                                    parikhMatrixValJ [y] [a,b,ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] | y == 'a' = [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij]
                                                                        | y == 'b' = [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij]
                                                                        | y == 'c' = [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij]
                                                                        | y == 'd' = [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'e' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'f' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'g' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g+1, fg+f, efg+ef, defg+def, cdefg+cdef, bcdefg+bcdef, abcdefg+abcdef, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij]
                                                                        | y == 'h' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h+1, gh+g, fgh+fg, efgh+efg, defgh+defg, cdefgh+cdefg, bcdefgh+bcdefg, abcdefgh+abcdefg, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij]
                                                                        | y == 'i' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i+1, hi+h, ghi+gh, fghi+fgh, efghi+efgh, defghi+defgh, cdefghi+cdefgh, bcdefghi+bcdefgh, abcdefghi+abcdefgh, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'j' = [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j+1, ij+i, hij+hi, ghij+ghi, fghij+fghi, efghij+efghi, defghij+defghi, cdefghij+cdefghi, bcdefghij+bcdefghi, abcdefghij+abcdefghi] 
                                                                        | otherwise = [0]
                                    parikhMatrixValJ (y:ys) [a,b,ab,c,bc,abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] | y == 'a' = parikhMatrixValJ ys [a+1, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij]
                                                                        | y == 'b' = parikhMatrixValJ ys [a, b+1, ab+a, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij]
                                                                        | y == 'c' = parikhMatrixValJ ys [a, b, ab, c+1, bc+b, abc+ab, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij]
                                                                        | y == 'd' = parikhMatrixValJ ys [a, b, ab, c, bc, abc, d+1, cd+c, bcd+bc, abcd+abc, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'e' = parikhMatrixValJ ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e+1, de+d, cde+cd, bcde+bcd, abcde+abcd, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'f' = parikhMatrixValJ ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f+1, ef+e, def+de, cdef+cde, bcdef+bcde, abcdef+abcde, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'g' = parikhMatrixValJ ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g+1, fg+f, efg+ef, defg+def, cdefg+cdef, bcdefg+bcdef, abcdefg+abcdef, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'h' = parikhMatrixValJ ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h+1, gh+g, fgh+fg, efgh+efg, defgh+defg, cdefgh+cdefg, bcdefgh+bcdefg, abcdefgh+abcdefg, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'i' = parikhMatrixValJ ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i+1, hi+h, ghi+gh, fghi+fgh, efghi+efgh, defghi+defgh, cdefghi+cdefgh, bcdefghi+bcdefgh, abcdefghi+abcdefgh, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] 
                                                                        | y == 'j' = parikhMatrixValJ ys [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j+1, ij+i, hij+hi, ghij+ghi, fghij+fghi, efghij+efghi, defghij+defghi, cdefghij+cdefghi, bcdefghij+bcdefghi, abcdefghij+abcdefghi] 
                                                                        | otherwise = [0] 
                                    parikhMatrixValJ _ _ = [0] 
parikhMatrixValues _ _ | otherwise = [0]           
--------------------------------------------------------------------------------
--end of parikhMatrixElements of size 10             
             
             
             
              










--------------------------------------------------------------------------------             
--indexTable for a given alphabet length maximal 10
-- a,b,ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij
--0
--1
--2
-- vdots
--length of String

-- return the column x of the indexTable of the word s. The first 0 is the initial from position 0
getColumnIndexTable :: String -> Int -> [Int]
getColumnIndexTable s x = getColumnIndexTableHelp x (indexTableWord s)

--case the table don't have enough entries
zeroColumn :: Int -> [Int]                           
zeroColumn 0 = [0]
zeroColumn c = [0] ++ zeroColumn (c-1)                     
                           
--get the Column y of a given pair of string and indexTable
getColumnIndexTableHelp :: Int -> (String,[[Int]]) -> [Int]
getColumnIndexTableHelp _ (_,[[]]) = []
getColumnIndexTableHelp y (z,[ys]) | y > (length ys) = zeroColumn (length z)
                                   | otherwise = [(ys !! (y-1))]
getColumnIndexTableHelp y (z,(xs:ys)) | y > (length xs) = zeroColumn (length z)
                                      | otherwise = [(xs !! (y-1))] ++ (getColumnIndexTableHelp y (z,ys))
getColumnIndexTableHelp _ _ = [0]
                          

-- returns for a given word his indextable
indexTableWord :: String -> (String,[[Int]])
indexTableWord s = indexTable s (alphabetLengthAlphabetical s)

-- given string and alphabet size calculate table in length of the word.
-- number of occurences of a string at position i (row i) 
-- alphabet size depending on incIndex1
indexTable :: String -> Int -> (String,[[Int]])
indexTable s x | x == 0 = (s,[[0]])
               | otherwise = (s, (indexHelp1 s (init1(gauß1 x)) [(init1(gauß1 x))])) where
                                   indexHelp1:: String -> [Int] -> [[Int]] -> [[Int]]
                                   indexHelp1 [] _ n2 = n2
                                   indexHelp1 [y] n1 n2 = n2 ++ [incIndex1 y n1]
                                   indexHelp1 (y:ys) n1 n2 = indexHelp1 ys (incIndex1 y n1) (n2 ++ [incIndex1 y n1])


--calculate number of table colums
gauß1 :: Int -> Int
gauß1 n = div (n * (n+1)) 2

--first row of zeros
init1 :: Int -> [Int]
init1 n | n==0 = []
        | otherwise = [0] ++ init1 (n-1)

-- gets a letter of a position and a list of all occurences of the row before and increase the value according to the position
incIndex1:: Char -> [Int] -> [Int]
incIndex1 y ys | y == 'a' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (x1:xs) = ((x1+1):xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'b' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (x1:x2:x3:xs) = (x1:(x2+1):(x3+x1):xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'c' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (x1:x2:x3:x4:x5:x6:xs) = (x1:x2:x3:(x4+1):(x5+x2):(x6+x3):xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'd' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (a:b:ab:c:bc:abc:d:cd:bcd:abcd:xs) = (a:b:ab:c:bc:abc:d+1:cd+c:bcd+bc:abcd+abc:xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'e' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde:xs) = (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e+1:de+d: cde+cd:bcde+bcd:abcde+abcd:xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'f' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde:f:ef:def:cdef:bcdef:abcdef:xs) = (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde:f+1:ef+e:def+de:cdef+cde:bcdef+bcde:abcdef+abcde:xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'g' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde:f:ef:def:cdef:bcdef:abcdef:g:fg: efg:defg:cdefg:bcdefg:abcdefg:xs) = (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde:f:ef:def:cdef:bcdef:abcdef:g+1:fg+f:efg+ef:defg+def:cdefg+cdef: bcdefg+bcdef:abcdefg+abcdef:xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'h' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde:f:ef:def:cdef:bcdef:abcdef:g:fg:efg:defg:cdefg:bcdefg:abcdefg: h:gh:fgh:efgh:defgh:cdefgh:bcdefgh:abcdefgh:xs) = (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde:f:ef:def:cdef:bcdef:abcdef:g:fg:efg:defg:cdefg:bcdefg:abcdefg: h+1:gh+g:fgh+fg:efgh+efg:defgh+defg:cdefgh+cdefg:bcdefgh+bcdefg:abcdefgh+abcdefg:xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'i' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde:f:ef:def:cdef:bcdef:abcdef:g:fg:efg:defg:cdefg:bcdefg:abcdefg: h:gh:fgh:efgh:defgh:cdefgh:bcdefgh:abcdefgh:i:hi:ghi:fghi:efghi:defghi:cdefghi:bcdefghi:abcdefghi:xs) = (a:b:ab:c:bc:abc:d:cd:bcd:abcd:e:de:cde:bcde:abcde: f:ef:def:cdef:bcdef:abcdef: g:fg:efg:defg:cdefg:bcdefg:abcdefg: h:gh:fgh:efgh:defgh:cdefgh:bcdefgh:abcdefgh: i+1:hi+h:ghi+gh:fghi+fgh:efghi+efgh:defghi+defgh:cdefghi+cdefgh:bcdefghi+bcdefgh:abcdefghi+abcdefgh:xs)
                            incIndex2 xs = xs
incIndex1 y ys | y == 'j' = incIndex2 ys where
                            incIndex2 :: [Int] -> [Int]
                            incIndex2 (a:b:ab:c:bc:abc:d:cd:bcd:abcd: e:de:cde:bcde:abcde: f:ef:def:cdef:bcdef:abcdef: g:fg:efg:defg:cdefg:bcdefg:abcdefg: h:gh:fgh:efgh:defgh:cdefgh:bcdefgh:abcdefgh: i:hi:ghi:fghi:efghi:defghi:cdefghi:bcdefghi:abcdefghi: j:ij:hij:ghij:fghij:efghij:defghij:cdefghij:bcdefghij:abcdefghij:xs) = (a:b:ab:c:bc:abc:d:cd:bcd:abcd: e:de:cde:bcde:abcde: f:ef:def:cdef:bcdef:abcdef: g:fg:efg:defg:cdefg:bcdefg:abcdefg: h:gh:fgh:efgh:defgh:cdefgh:bcdefgh:abcdefgh: i:hi:ghi:fghi:efghi:defghi:cdefghi:bcdefghi:abcdefghi: j+1:ij+i:hij+hi:ghij+ghi:fghij+fghi:efghij+efghi:defghij+efghi:cdefghij+cdefghi:bcdefghij+bcdefghi:abcdefghij+abcdefghi:xs) 
                            incIndex2 xs = xs
incIndex1 _ _ | otherwise = [0]


--------------------------------------------------------------------------------
--end of indexTable of size 10 

--------------------------------------------------------------------------------
-- for a b c d e f g h i j


mHelp10 :: [Int] -> [String]
mHelp10 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi, j, ij, hij, ghij, fghij, efghij, defghij, cdefghij, bcdefghij, abcdefghij] | j == 0 = mHelp9 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi]
                       | otherwise = mHelp10' (mHelp9 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] ) [j, i, ij, hi, hij, ghi, ghij, fghi, fghij, efghi, efghij, defghi, defghij, cdefghi, cdefghij, bcdefghi, bcdefghij, abcdefghi, abcdefghij]
mHelp10 _ = ["wrong input mHelp1?"]


-- shuffle words old
mHelp10' :: [String] ->[Int] -> [String]
mHelp10' [] _ = []
mHelp10' [""] (0:_)= [""]
--mHelp10' [""] (j:_) = nub (shuffleWords "" (helpNum "j" j))
mHelp10' [""] (j:_) = shuffle42 "" (helpNum "j" j)
mHelp10' [x] [j, i, ij, hi, hij, ghi, ghij, fghi, fghij, efghi, efghij, defghi, defghij, cdefghi, cdefghij, bcdefghi, bcdefghij, abcdefghi, abcdefghij] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions10 x [j, i, ij, hi, hij, ghi, ghij, fghi, fghij, efghi, efghij, defghi, defghij, cdefghi, cdefghij, bcdefghi, bcdefghij, abcdefghi, abcdefghij]) j))))) "j")
mHelp10' (x:xs) [j, i, ij, hi, hij, ghi, ghij, fghi, fghij, efghi, efghij, defghi, defghij, cdefghi, cdefghij, bcdefghi, bcdefghij, abcdefghi, abcdefghij] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions10 x [j, i, ij, hi, hij, ghi, ghij, fghi, fghij, efghi, efghij, defghi, defghij, cdefghi, cdefghij, bcdefghi, bcdefghij, abcdefghi, abcdefghij]) j))))) "j") ++ (mHelp10' (xs) [j, i, ij, hi, hij, ghi, ghij, fghi, fghij, efghi, efghij, defghi, defghij, cdefghi, cdefghij, bcdefghi, bcdefghij, abcdefghi, abcdefghij])
mHelp10' _ _ = ["wrong input mHelp1?"]

-- convert all Partitions to a list of possible Positions
partitionToIndexPositions10 :: String -> [Int] -> [[[(Int,Int)]]] 
partitionToIndexPositions10 s [j, i, ij, hi, hij, ghi, ghij, fghi, fghij, efghi, efghij, defghi, defghij, cdefghi, cdefghij, bcdefghi, bcdefghij, abcdefghi, abcdefghij] = [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength ij i j) j) (getColumnIndexTableHelp 37 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 37 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength hij hi j) j) (getColumnIndexTableHelp 38 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 38 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength ghij ghi j) j) (getColumnIndexTableHelp 39 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 39 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength fghij fghi j) j) (getColumnIndexTableHelp 40 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 40 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength efghij efghi j) j) (getColumnIndexTableHelp 41 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 41 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength defghij defghi j) j) (getColumnIndexTableHelp 42 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 42 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength cdefghij cdefghi j) j) (getColumnIndexTableHelp 43 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 43 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength bcdefghij bcdefghi j) j) (getColumnIndexTableHelp 44 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 44 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength abcdefghij abcdefghi j) j) (getColumnIndexTableHelp 45 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 45 (indexTableWord s)))]
partitionToIndexPositions10 _ _ = [[[(909,404)]]]

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- for a b c d e f g h i


mHelp9 :: [Int] -> [String]
mHelp9 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh, i, hi, ghi, fghi, efghi, defghi, cdefghi, bcdefghi, abcdefghi] | i == 0 = mHelp8 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]
                                                                                                       | otherwise = mHelp9' (mHelp8 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh]) [i, h, hi, gh, ghi, fgh, fghi, efgh, efghi, defgh, defghi, cdefgh, cdefghi, bcdefgh, bcdefghi, abcdefgh, abcdefghi]
mHelp9 _ = ["wrong input mHelp1?"]

-- shuffle words old
mHelp9' :: [String] ->[Int] -> [String]
mHelp9' [] _ = []
mHelp9' [""] (0:_)= [""]
--mHelp9' [""] (i:_) = nub (shuffleWords "" (helpNum "i" i))
mHelp9' [""] (i:_) = shuffle42 "" (helpNum "i" i)
mHelp9' [x] [i, h, hi, gh, ghi, fgh, fghi, efgh, efghi, defgh, defghi, cdefgh, cdefghi, bcdefgh, bcdefghi, abcdefgh, abcdefghi] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions9 x [i, h, hi, gh, ghi, fgh, fghi, efgh, efghi, defgh, defghi, cdefgh, cdefghi, bcdefgh, bcdefghi, abcdefgh, abcdefghi]) i))))) "i")
mHelp9' (x:xs) [i, h, hi, gh, ghi, fgh, fghi, efgh, efghi, defgh, defghi, cdefgh, cdefghi, bcdefgh, bcdefghi, abcdefgh, abcdefghi] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions9 x [i, h, hi, gh, ghi, fgh, fghi, efgh, efghi, defgh, defghi, cdefgh, cdefghi, bcdefgh, bcdefghi, abcdefgh, abcdefghi]) i))))) "i") ++ (mHelp9' (xs) [i, h, hi, gh, ghi, fgh, fghi, efgh, efghi, defgh, defghi, cdefgh, cdefghi, bcdefgh, bcdefghi, abcdefgh, abcdefghi])
mHelp9' _ _ = ["wrong input mHelp1?"]

-- convert all Partitions to a list of possible Positions
partitionToIndexPositions9 :: String -> [Int] -> [[[(Int,Int)]]] 
partitionToIndexPositions9 s [i, h, hi, gh, ghi, fgh, fghi, efgh, efghi, defgh, defghi, cdefgh, cdefghi, bcdefgh, bcdefghi, abcdefgh, abcdefghi] = [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength hi h i) i) (getColumnIndexTableHelp 29 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 29 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength ghi gh i) i) (getColumnIndexTableHelp 30 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 30 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength fghi fgh i) i) (getColumnIndexTableHelp 31 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 31 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength efghi efgh i) i) (getColumnIndexTableHelp 32 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 32 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength defghi defgh i) i) (getColumnIndexTableHelp 33 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 33 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength cdefghi cdefgh i) i) (getColumnIndexTableHelp 34 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 34 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength bcdefghi bcdefgh i) i) (getColumnIndexTableHelp 35 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 35 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength abcdefghi abcdefgh i) i) (getColumnIndexTableHelp 36 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 36 (indexTableWord s)))]
partitionToIndexPositions9 _ _ = [[[(909,404)]]]

--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- for a b c d e f g h


mHelp8 :: [Int] -> [String]
mHelp8 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg, h, gh, fgh, efgh, defgh, cdefgh, bcdefgh, abcdefgh] | h == 0 = mHelp7 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg]
                                        | otherwise = mHelp8' (mHelp7 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg]) [h, g, gh, fg, fgh, efg, efgh, defg, defgh, cdefg, cdefgh, bcdefg, bcdefgh, abcdefg, abcdefgh]
mHelp8 _ = ["wrong input mHelp1?"]

mHelp8' :: [String] ->[Int] -> [String]
mHelp8' [] _ = []
mHelp8' [""] (0:_)= [""]
--mHelp8' [""] (h:_) = nub (shuffleWords "" (helpNum "h" h))
mHelp8' [""] (h:_) = shuffle42 "" (helpNum "h" h)
mHelp8' [x] [h, g, gh, fg, fgh, efg, efgh, defg, defgh, cdefg, cdefgh, bcdefg, bcdefgh, abcdefg, abcdefgh] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions8 x [h, g, gh, fg, fgh, efg, efgh, defg, defgh, cdefg, cdefgh, bcdefg, bcdefgh, abcdefg, abcdefgh]) h))))) "h")
mHelp8' (x:xs) [h, g, gh, fg, fgh, efg, efgh, defg, defgh, cdefg, cdefgh, bcdefg, bcdefgh, abcdefg, abcdefgh] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions8 x [h, g, gh, fg, fgh, efg, efgh, defg, defgh, cdefg, cdefgh, bcdefg, bcdefgh, abcdefg, abcdefgh]) h))))) "h") ++ (mHelp8' (xs) [h, g, gh, fg, fgh, efg, efgh, defg, defgh, cdefg, cdefgh, bcdefg, bcdefgh, abcdefg, abcdefgh])
mHelp8' _ _ = ["wrong input mHelp1?"]

-- convert all Partitions to a list of possible Positions
partitionToIndexPositions8 :: String -> [Int] -> [[[(Int,Int)]]] 
partitionToIndexPositions8 s [h, g, gh, fg, fgh, efg, efgh, defg, defgh, cdefg, cdefgh, bcdefg, bcdefgh, abcdefg, abcdefgh] = [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength gh g h) h) (getColumnIndexTableHelp 22 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 22 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength fgh fg h) h) (getColumnIndexTableHelp 23 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 23 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength efgh efg h) h) (getColumnIndexTableHelp 24 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 24 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength defgh defg h) h) (getColumnIndexTableHelp 25 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 25 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength cdefgh cdefg h) h) (getColumnIndexTableHelp 26 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 26 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength bcdefgh bcdefg h) h) (getColumnIndexTableHelp 27 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 27 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength abcdefgh abcdefg h) h) (getColumnIndexTableHelp 28 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 28 (indexTableWord s)))]
partitionToIndexPositions8 _ _ = [[[(909,404)]]]

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- for a b c d e f g


mHelp7 :: [Int] -> [String]
mHelp7 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef, g, fg, efg, defg, cdefg, bcdefg, abcdefg] | g == 0 = mHelp6 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]
                                                                                                                                                    | otherwise = mHelp7' (mHelp6 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]) [g, f, fg, ef, efg, def, defg, cdef, cdefg, bcdef, bcdefg, abcdef, abcdefg]
mHelp7 _ = ["wrong input mHelp1?"]

mHelp7' :: [String] ->[Int] -> [String]
mHelp7' [] _ = []
mHelp7' [""] (0:_)= [""]
--mHelp7' [""] (g:_) = nub (shuffleWords "" (helpNum "g" g))
mHelp7' [""] (g:_) = shuffle42 "" (helpNum "g" g)
mHelp7' [x] [g, f, fg, ef, efg, def, defg, cdef, cdefg, bcdef, bcdefg, abcdef, abcdefg] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions7 x [g, f, fg, ef, efg, def, defg, cdef, cdefg, bcdef, bcdefg, abcdef, abcdefg]) g))))) "g")
mHelp7' (x:xs) [g, f, fg, ef, efg, def, defg, cdef, cdefg, bcdef, bcdefg, abcdef, abcdefg] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions7 x [g, f, fg, ef, efg, def, defg, cdef, cdefg, bcdef, bcdefg, abcdef, abcdefg]) g))))) "g") ++ (mHelp7' (xs) [g, f, fg, ef, efg, def, defg, cdef, cdefg, bcdef, bcdefg, abcdef, abcdefg])
mHelp7' _ _ = ["wrong input mHelp1?"]

-- convert all Partitions to a list of possible Positions
partitionToIndexPositions7 :: String -> [Int] -> [[[(Int,Int)]]] 
partitionToIndexPositions7 s [g, f, fg, ef, efg, def, defg, cdef, cdefg, bcdef, bcdefg, abcdef, abcdefg] = [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength fg f g) g) (getColumnIndexTableHelp 16 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 16 (indexTableWord s)))] ++[indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength efg ef g) g) (getColumnIndexTableHelp 17 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 17 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength defg def g) g) (getColumnIndexTableHelp 18 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 18 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength cdefg cdef g) g) (getColumnIndexTableHelp 19 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 19 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength bcdefg bcdef g) g) (getColumnIndexTableHelp 20 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 20 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength abcdefg abcdef g) g) (getColumnIndexTableHelp 21 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 21 (indexTableWord s)))]
partitionToIndexPositions7 _ _ = [[[(909,404)]]]

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- for a b
mHelp3 :: [Int] -> [String]
mHelp3 [a, b, ab] | a == 0 && b == 0 = [""]
                  | otherwise = partitionListToString (partitionMaxElemMaxLength ab a b) a b
mHelp3 _ = ["wrong input mHelp1?"]

--------------------------------------------------------------------------------
-- for a b c d e f


mHelp6 :: [Int] -> [String]
mHelp6 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde, f, ef, def, cdef, bcdef, abcdef]  | f == 0 = mHelp5 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde]
                                                                                                           | otherwise = mHelp6' (mHelp5 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde]) [f, e, ef, de, def, cde, cdef, bcde, bcdef, abcde, abcdef]
mHelp6 _ = ["wrong input mHelp1?"]

mHelp6' :: [String] ->[Int] -> [String]
mHelp6' [] _ = []
mHelp6' [""] (0:_)= [""]
--mHelp6' [""] (f:_) = nub (shuffleWords "" (helpNum "f" f))
mHelp6' [""] (f:_) = shuffle42 "" (helpNum "f" f)
mHelp6' [x] [f, e, ef, de, def, cde, cdef, bcde, bcdef, abcde, abcdef] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions6 x [f, e, ef, de, def, cde, cdef, bcde, bcdef, abcde, abcdef]) f))))) "f")
mHelp6' (x:xs) [f, e, ef, de, def, cde, cdef, bcde, bcdef, abcde, abcdef] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions6 x [f, e, ef, de, def, cde, cdef, bcde, bcdef, abcde, abcdef]) f))))) "f") ++ (mHelp6' (xs) [f, e, ef, de, def, cde, cdef, bcde, bcdef, abcde, abcdef])
mHelp6' _ _ = ["wrong input mHelp1?"]

-- convert all Partitions to a list of possible Positions
partitionToIndexPositions6 :: String -> [Int] -> [[[(Int,Int)]]] 
partitionToIndexPositions6 s [f, e, ef, de, def, cde, cdef, bcde, bcdef, abcde, abcdef] = [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength ef e f) f) (getColumnIndexTableHelp 11 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 11 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength def de f) f) (getColumnIndexTableHelp 12 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 12 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength cdef cde f) f) (getColumnIndexTableHelp 13 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 13 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength bcdef bcde f) f) (getColumnIndexTableHelp 14 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 14 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength abcdef abcde f) f) (getColumnIndexTableHelp 15 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 15 (indexTableWord s)))]
partitionToIndexPositions6 _ _ = [[[(909,404)]]]

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- for a b c d e


mHelp5 :: [Int] -> [String]
mHelp5 [a, b, ab, c, bc, abc, d, cd, bcd, abcd, e, de, cde, bcde, abcde] | e == 0 = mHelp4 [a, b, ab, c, bc, abc, d, cd, bcd, abcd]
                                                                         | otherwise = mHelp5' (mHelp4 [a, b, ab, c, bc, abc, d, cd, bcd, abcd]) [e, d, de, cd, cde, bcd, bcde, abcd, abcde]
mHelp5 _ = ["wrong input mHelp1?"]

mHelp5' :: [String] ->[Int] -> [String]
mHelp5' [] _ = []
mHelp5' [""] (0:_)= [""]
--mHelp5' [""] (e:_) = nub (shuffleWords "" (helpNum "e" e))
mHelp5' [""] (e:_) = shuffle42 "" (helpNum "e" e)
mHelp5' [x] [e, d, de, cd, cde, bcd, bcde, abcd, abcde] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions5 x [e, d, de, cd, cde, bcd, bcde, abcd, abcde]) e))))) "e")
mHelp5' (x:xs) [e, d, de, cd, cde, bcd, bcde, abcd, abcde] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions5 x [e, d, de, cd, cde, bcd, bcde, abcd, abcde]) e))))) "e") ++ (mHelp5' (xs) [e, d, de, cd, cde, bcd, bcde, abcd, abcde])
mHelp5' _ _ = ["wrong input mHelp1?"]

-- convert all Partitions to a list of possible Positions
partitionToIndexPositions5 :: String -> [Int] -> [[[(Int,Int)]]] 
partitionToIndexPositions5 s [e, d, de, cd, cde, bcd, bcde, abcd, abcde] = [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength de d e) e) (getColumnIndexTableHelp 7 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 7 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength cde cd e) e) (getColumnIndexTableHelp 8 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 8 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength bcde bcd e) e) (getColumnIndexTableHelp 9 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 9 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength abcde abcd e) e) (getColumnIndexTableHelp 10 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 10 (indexTableWord s)))]
partitionToIndexPositions5 _ _ = [[[(909,404)]]]

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- for a b c d
--mEquivalentWords "abbac"
--["abbca","abbac","baabc"]
-- parikhMatrixValues "abbacd" 4
-- [2,2,2,1,2,2,1,1,2,2]
-- a,b,ab,c,bc,abc,d,cd,bcd,abcd


mHelp4 :: [Int] -> [String]
mHelp4 [a, b, ab, c, bc, abc, d, cd, bcd, abcd] | d == 0 = mHelp1 [a, b, ab, c, bc, abc]
                                                | otherwise = mHelp4' (mHelp1 [a,b, ab, c, bc, abc]) [d, c, cd, bc, bcd, abc, abcd]
mHelp4 _ = ["wrong input mHelp1?"]

mHelp4' :: [String] ->[Int] -> [String]
mHelp4' [] _ = []
mHelp4' [""] (0:_)= [""]
--mHelp4' [""] (d:_) = nub (shuffleWords "" (helpNum "d" d))
mHelp4' [""] (d:_) = shuffle42 "" (helpNum "d" d)
mHelp4' [x] [d, c, cd, bc, bcd, abc, abcd] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions4 x d c cd bc bcd abc abcd) d))))) "d")
mHelp4' (x:xs) [d, c, cd, bc, bcd, abc, abcd] = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions4 x d c cd bc bcd abc abcd) d))))) "d") ++ (mHelp4' (xs) [d, c, cd, bc, bcd, abc, abcd])
mHelp4' _ _ = ["wrong input mHelp1?"]

-- convert all Partitions to a list of possible Positions
partitionToIndexPositions4 :: String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[[(Int,Int)]]] 
partitionToIndexPositions4 s d c cd bc bcd abc abcd = [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength cd c d) d) (getColumnIndexTableHelp 4 (indexTableWord s))) (indexPositions (getColumnIndexTableHelp 4 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength bcd bc d) d) (getColumnIndexTableHelp 5 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 5 (indexTableWord s)))] ++ [indexPosition1 (helpReduction (partitionFillUp (partitionMaxElemMaxLength abcd abc d) d) (getColumnIndexTableHelp 6 (indexTableWord s)))(indexPositions( getColumnIndexTableHelp 6 (indexTableWord s)))]
-------------------------------------------------------------------------------

-- for a b c 
mHelp1 :: [Int] -> [String]
mHelp1 [a, b, ab, c, bc, abc] | c == 0 = mHelp3 [a, b, ab]
                              | otherwise = mHelp2 (partitionListToString (partitionMaxElemMaxLength ab a b) a b) c b bc ab abc
mHelp1 _ = ["wrong input mHelp1?"]

-- for a b c 
mHelp2 :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
mHelp2 [] _ _ _ _ _= []
mHelp2 [""] 0 _ _ _ _= [""]
--mHelp2 [""] c _ _ _ _= nub (shuffleWords "" (helpNum "c" c))
mHelp2 [""] c _ _ _ _= shuffle42 "" (helpNum "c" c)
mHelp2 [x] c b bc ab abc = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions x c b bc ab abc) c))))) "c")
mHelp2 (x:xs) c b bc ab abc = productWord1(regExp1 (compressionProductOfWordsForIndex1 (productOfWordsForIndex x (helpR1 (indexNumbers (intersectPosList (partitionToIndexPositions x c b bc ab abc) c))))) "c") ++ (mHelp2 (xs) c b bc ab abc)

--reverse the list so the smallest are in front
helpR1 :: [[(Int,Int,Int)]] -> [[(Int,Int,Int)]]
helpR1 [] = []
helpR1 [x] = [helpR4 x]
helpR1 (xs:ys) = [(helpR4 xs)] ++ (helpR1 ys)

helpR4 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
helpR4 [] = []
helpR4 [x] = [x]
helpR4 (x:xs) = (helpR4 xs) ++ [x]  

