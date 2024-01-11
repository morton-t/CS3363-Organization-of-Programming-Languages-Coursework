{-

Compile using `ghc hasqs` or `ghci hasqs.hs`

Program can be ran interactively by using the following commands:
     '
	ghci
	:l hasqs

	hasqs [INSERT UNSORTED LIST HERE]
     '

~OR~

     '
	ghc -e "hasqs [INSERT UNSORTED LIST HERE" hasqs.hs
     '


 Program accepts a list of real values a0 .. an  within brackets and returns the sorted list
 with duplcate values removed. Sorting algorithm is quicksort by use of list comprehensions.
-}

module Hasqs where --Declares Hasqs as the main module

hasqs [] = [] -- Base case 0: An empty list is already sorted
hasqs [x] = [x] -- Base case 1: A list with one element is already sorted
{-
   We begin recursively sorting around a pivot 'x', where the LHS of the pivot is recursively sorted to be less than
   the pivot, and the RHS of the pivot is sorted to be greater than the pivot. Elements that are equal to the pivot are excluded
   from list concatenation as the recursive stack resolves. This ensures that duplucate elements are removed during the sort.
-}
hasqs (x:tail) = hasqs [pivot | pivot <- tail, pivot < x] ++ [x] ++ hasqs [pivot | pivot <- tail, pivot > x]
