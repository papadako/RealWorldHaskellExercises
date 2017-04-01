-- Exercises in chapter 03
module Exercise3 where

import           Data.List (sortBy)


{-
1. Write a function that computes the number of elements
in a list. To test it, ensure that it gives the same results
as the standard length function.
2. Add a type signature for your function to your source file.
To test it, load the source file into ghci again.
-}
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = myLength xs + 1

-- code to test mylength
ok2_1 = 0 == myLength []
ok2_2 = let l1 = [1,2,3]
      in  length l1 == myLength l1

{- 3. Write a function that computes the mean of a list, i.e. the sum
  of all elements in the list divided by its length. (You may need to use
  the fromIntegral function to convert the length of the list from an
  integer to a floating point number)
-}
myMean :: [Integer] -> Double
myMean [] = 0.0
myMean x  = fromIntegral(sumList x) / fromIntegral(Prelude.length x)

sumList :: [Integer] -> Integer
sumList(x:xs) = x + sumList xs
sumList []    = 0

{- 4. Turn a list into a palindrome; i.e. it should read the same
both backward and forward. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1]
-}

myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse []     = []

palindrome :: [a] -> [a]
palindrome x = x ++ myReverse x
-- test
ok4 = palindrome [1,2,3] == [1,2,3,3,2,1]

{- 5. Write a function that determines whether its input list is a palindrome-}
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome list = list == reverse list
-- test
ok5 = checkPalindrome (palindrome [1,2,3])

{- 6. Create a function that sorts a list of lists based on the length of each
sublist. You may want to look at the sortBy function from Data.List module. -}

sortListsOnLength :: [[a]] -> [[a]]
sortListsOnLength = sortBy listLengthComparator

listLengthComparator :: [a] -> [a] -> Ordering
listLengthComparator a b = case compare (length a) (length b) of
                          EQ -> EQ
                          LT -> GT
                          GT -> LT
--test
ok6 = sortListsOnLength [[1,2],[1],[1,2,3]] == [[1,2,3],[1,2],[1]]

{- 7. Define a function that joins a list of lists together using a separator value
intersperse :: a -> [[a]] -> [a]
The separator should appear between elements of the list, but it should not follow
the last element. Your function should behave as follows:

-}
intersperse :: a -> [[a]] -> [a]
intersperse sep (x:xs)
  | length xs >= 1 = x ++ [sep] ++ intersperse sep xs
  | null xs       = x
intersperse _ [] = []

-- test
ok7_1 = intersperse ',' [] == ""
ok7_2 = intersperse ',' ["foo"] == "foo"
ok7_3 = intersperse ',' ["foo", "bar", "baz", "quux"] == "foo,bar,baz,quux"

{- 8. Using the binary tree type that we defined earlier in this chapter, write
a function that will determine the height of the tree. The height is the largest
number of hops from the root to an Empty. For example, the tree Empty has height
zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty)
has height two; and so on.
-}
