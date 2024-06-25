-- #1 last elem
myLast [] = Nothing
myLast (x : []) = Just x
myLast (x : xs) = myLast xs
-- main = putStrLn (show (myLast [1, 2, 3, 4]))

-- #2 second last elem
myButLast [] = Nothing
myButLast (x : []) = Nothing
myButLast (x1 : x2 : []) = Just x1
myButLast (x : xs) = myLast xs
-- main = putStrLn (show (myButLast ['a'..'z']))

-- #3 Find the K'th element of a list.
elementAt [] a = Nothing
elementAt (x:xs) 1 = Just x
elementAt (x:xs) a = elementAt xs (a-1)
-- main = putStrLn (show (elementAt "haskell" 5))

-- #4 Find the number of elements in a list.
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
-- main = putStrLn (show (myLength "Hello, world!"))

-- #5 Reverse a list.
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
-- main = putStrLn (show (myReverse "A man, a plan, a canal, panama!"))

-- #6 Find out whether a list is a palindrome.
isPalindrome xs = xs == myReverse xs
-- main = putStrLn (show (isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1]))

-- #7 Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten (List []) = []
flatten (List (Elem a : as)) = a : flatten (List as)
flatten (List (List a : as)) = flatten (List a) ++ flatten (List as)
-- main = putStrLn (show (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])))

-- #8 Eliminate consecutive duplicates of list elements.
compress [] = []
compress (a : []) = a : []
compress (a : b : xs) = if a == b then compress (b : xs) else (a : compress (b : xs))
-- main = putStrLn (show (compress "aaaabccaadeeee"))

-- #9 Pack consecutive duplicates of list elements into sublists.
packR ch acc res [] = res ++ [acc]
packR ch acc res (c : cs) = if c == ch 
        then (packR c (acc ++ [c]) res cs)
        else (packR c [c] (res ++ [acc]) cs)

pack [] = []
pack (a : as) = packR a [a] [] as 
-- main = putStrLn (show (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']))

-- #10 Run-length encoding of a list.
len [] = 0
len (a : as) = (len as) + 1
countAndChar (a : as) = (len(as) + 1, a)

encode [] = []
encode a = map countAndChar (pack a)

main = putStrLn (show (encode "aaaabccaadeeee"))
