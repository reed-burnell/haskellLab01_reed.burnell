-- problem 1
factorial n = product [1..n]
-- problem 2
middle ns = drop 1 (reverse(drop 1 (reverse ns)))
-- problem 3
secondToLast ns = ns!!((length ns) - 2)
-- problem 4
halfOf ns = take (((length(ns)) `div` 2)) ns
-- problem 5
palindromify ns = ns ++ (reverse(take ((length(ns)) - 1) ns))
-- problem 6
app3 xs ys zs = (reverse zs) ++ (reverse ys) ++ (reverse xs)
-- problem 7
myFunc ns = drop (head ns) ns
-- problem 8
problem8 ns = take (maximum ns) (cycle[minimum ns,minimum ns])
-- problem 9
problem9 n ns = take (n * length ns) (cycle ns)
-- problem 10
countBy x y = [n*x | n <- [1..y]]
-- problem 11
number11 ns = take (((maximum ns) - (minimum ns))*2) (cycle [minimum ns,maximum ns])
-- problem 12
-- median :: [Double] -> Double
median ns = if (k `mod` 2) == 0
    then ( (g+h) / 2)
    else h
    where
     k = (length ns)
     j = (k `div` 2)
     g = (ns!!(j-1))
     h = (ns!!j)
