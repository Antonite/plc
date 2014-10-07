--Anton Pashyk
--sieve function taken from Sieve of Eratosthenes wiki http://en.literateprograms.org/Sieve_of_Eratosthenes_%28Haskell%29
--i used ":l project3" inside ghci to execute

import Data.List

insertInAll a lst = map insertone lst
	where
		insertone alist = a:alist

maxElement lst = last (sort lst)

mkcopy x = [x] ++ [x]
duplicate lst = concatMap mkcopy lst

nothing x = x
flatten lst = concatMap nothing lst

sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
primes a = nub (map isless (take a (sieve [2..])))
    where
	isless x = if x<a then x else 2


isMember2 x lst = foldr (||) False (map memchk lst)
	where
		memchk a = if x==a then True else False

union2 set1 set2 = set1 ++ set2

intersect2 set1 set2 = filter (/= -999999) (map simchk set1)
	where
		simchk x = if (isMember2 x set2) then x else -999999

diff2 set1 set2 = filter (/= -999999) (map simchk set1)
	where
		simchk x = if (not (isMember2 x set2)) then x else -999999

symmDiff2 set1 set2 = filter (/= -999999) ((map simchk set1) ++ (map simchk2 set2))
	where
		simchk x = if (not (isMember2 x set2)) then x else -999999;
		simchk2 x = if (not (isMember2 x set1)) then x else -999999


areEqual2 [] [] = True
areEqual2 set1 set2 = if (length set1) == (length set2) 
			then (if (head set1) == (head set2) then (areEqual2 (tail set1) (tail set2)) else False)
			else False
