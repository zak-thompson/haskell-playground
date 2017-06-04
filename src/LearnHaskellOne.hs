module LearnHaskellOne where
import Data.List

--3rd question's function
kThElement :: [t] -> Int -> t
kThElement list k = list !! (k-1)

--6th question's function
isPalindrome :: Eq t => [t] -> Bool
isPalindrome list = (length list == 1 || null list) || ((head list == last list) && isPalindrome (tail (init list)))

--7th question's function
data NestedList a = Elem a | List [NestedList a]
flattenNestedStructure :: NestedList a -> [a]
flattenNestedStructure (Elem x) = [x]
flattenNestedStructure (List x) = concatMap flattenNestedStructure x

--10th question's function
condense :: [t] -> (Int, t)
condense list = (length list, head list)
encode :: Eq t => [t] -> [(Int, t)]
encode list = map condense (group list)

--11th question's function
data ListItem a = Single a | Multiple Int a deriving Show

condense2 :: [t] -> ListItem t
condense2 list = if length list == 1 then Single (head list) else Multiple (length list) (head list)

encode2 :: Eq t => [t] -> [ListItem t]
encode2 list = map condense2 (group list)

--12th question's function
decodeItem :: ListItem t -> [t]
decodeItem (Single x) = [x]
decodeItem (Multiple x y) = replicate x y

decode2 :: [ListItem t] -> [t]
decode2 list = list >>= decodeItem

--found on stack overflow for isPalindrome when looking up proper if statements/control flow for Haskell, what the fuck
--getting compile error when I try it, TODO investigate
--isPalindrome2 :: Eq t => [t] -> Bool
--isPalindrome2 list = reverse >>= (==)

ninetyNineQuestions :: IO ()
ninetyNineQuestions = do
    let listOne = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    -- Q1: last element of list
    print (last listOne)
    -- Q2: last minus one element of list
    print (last (init listOne))
    -- Q3: kth element
    print (kThElement listOne 3)
    --Q4: #elements in list
    print (length listOne)
    --Q5 reverse list
    print (reverse listOne)
    --Q6: is palindrome
    print (isPalindrome listOne)
    --Q7: nested flatten
    print (flattenNestedStructure (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    --Q8: remove duplicates
    print (Data.List.nub "aaaabccaadeeee")
    --Q9: Pack consecutive duplicates of list elements into sublists
    print (group ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
    --Q10: Run-length encoding of a list
    print (encode ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
    --Q11: Run-length encoding of a list a second time
    print (encode2 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
    --Q12: Decode given run-length encoding
    print (decode2 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'])
    
