module Tut1 where

f x = x + y
     where y = 7


g x = let y = 7 in x + y where y = 10000




numberOfEvenElements [] = 0

numberOfEvenElements (x:xs)
 | (even x) = 1 + numberOfEvenElements xs
 | otherwise = numberOfEvenElements xs

sumOfEvenElements n = sum [x | x <- [1..n], even x]


schnitt daten = sum noten / (fromIntegral . length) daten
     where noten = map snd daten


schnitt2 daten = sum notenBestanden  / (fromIntegral . length) notenBestanden
     where notenBestanden = filter(<= 4.0) noten
           noten          = map snd daten
           
schnitt3 daten = schnitt (filter ((<= 4.0) .snd) daten)




data Location = Address String Int String Int String | GPS Double Double

isInKalsruhe :: Location -> Bool
isInKalsruhe (Address _ _ _ code city) = (code >= 76131 && code <= 76229 && city == "Karlsruhe")


data Tree a = Leaf  | Node (Tree a) a (Tree a)

insert :: (Ord a) => a -> Tree a -> Tree abs
insert x Leaf = Node Leaf x Leaf
insert x (Node t1 y t2) 
 | (x <= y) = Node (insert x t1) y t2
 | (otherwise) = Node t1 y (insert x t2)