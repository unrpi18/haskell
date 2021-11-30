module List where

myLast [] = error "negative index!!"
myLast xs = xs !! (length xs - 1)

elementAt xs n = xs !! (n - 1)


myLength [] = 0
myLength (_:xs)  = 1 + myLength xs

{-
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]
-}

myReverse xs = reverse' xs []
  where
     reverse' []  reversed =  reversed
     reverse' (x:xs) reversed = reverse' xs (x:reversed)


isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)


compress xs = compressAcc xs []
    where compressAcc [] acc = acc
