-- #9 Pack consecutive duplicates of list elements into sublists.
packR ch acc res [] = res ++ [acc]
packR ch acc res (c : cs) = if c == ch 
        then (packR c (acc ++ [c]) res cs)
        else (packR c [c] (res ++ [acc]) cs)

pack [] = []
pack (a : as) = packR a [a] [] as 

-- #10 Run-length encoding of a list.
len [] = 0
len (a : as) = (len as) + 1
countAndChar (a : as) = (len(as) + 1, a)

encode [] = []
encode a = map countAndChar (pack a)

-- #11 Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
data Task11Data = Single Char | Multiple Int Char deriving Show

tast11 as = map (\(num, char) -> if num == 1 then Single char else Multiple num char) (encode as)

-- #12 Decode a run-length encoded list.
expandStr (Multiple num ch) = foldl (\acc x -> acc ++ [ch]) "" [1..num]
expandStr (Single ch) = [ch]
tast12Decode encoded = encoded >>= expandStr   
-- main = putStrLn (show (tast12Decode [(Multiple 4 'a'), Single 'b', Multiple 2 'c',  Multiple 2 'a',Single 'd',Multiple 4 'e']))

-- #13 already did

-- #14 Duplicate the elements of a list.
tast14Duplicate xs = xs >>= (\x -> [x, x])
-- main = putStrLn (show (tast14Duplicate [1, 2, 3]))

-- #15 Replicate the elements of a list a given number of times.
expand num item = map (\x -> item) [1..num]
tast15Replicate xs num = xs >>= (expand num) 
-- main = putStrLn (show (tast15Replicate "ABC" 3))

-- #16 Drop every N'th element from a list.
tast16Drop xs num = map (\(x, y) -> x) (filter (\(x, y) -> (mod y num) /= 0) (zip xs [1..]))
-- main = putStrLn (show (tast16Drop "abcdefghik" 3))

-- #17 Drop every N'th element from a list.
task17Sp num (acc1, acc2) (x, idx) = if idx > num then (acc1, acc2 ++ [x]) else (acc1 ++ [x], acc2)
task17Split xs num = foldl (task17Sp num) ([], []) (zip xs [1..])
-- main = putStrLn (show (task17Split "abcdefghik" 3))

-- #18 Extract a slice from a list.
task18Sl numMin numMax acc1 (x, idx) = if (idx > numMin - 1) && (idx < numMax + 1) then (acc1 ++ [x]) else acc1
task18Slice xs numMin numMax = foldl (task18Sl numMin numMax) [] (zip xs [1..])
-- main = putStrLn (show (task18Slice "abcdefghik" 3 7))
-- #19 Rotate a list N places to the left.

switch (a, b) = b ++ a
task19Rotate xs num 
    | num < 0 = switch (task17Split xs (length xs + num))
    | otherwise = switch (task17Split xs  num)
-- main = putStrLn (show (task19Rotate "abcdefgh" 3))
-- main = putStrLn (show (task19Rotate "abcdefgh" (-2)))

-- #20 Remove the K'th element from a list.
task20Remove 1 (x:xs) = (x, xs) 
task20Remove num (x:xs) = (bad, x:ys)
    where (bad, ys) = task20Remove (num - 1) xs

main = putStrLn (show (task20Remove 2 "abcd"))






