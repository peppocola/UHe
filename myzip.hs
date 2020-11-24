module MyZip where

myzip :: [x] -> [y] -> [(x,y)]              --This means that if I get two lists in input, one of type x and another of type y, in output I get a list of pairs where the first element is of type x and the second element is of type y
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys   --If this pattern matches, it means that both the lists have at least one element
myzip _ _ = []                              --This pattern is mathched only if the first doesnt match, and this means that at least one of the two lists is empty. So we cut off other values of the non empty list.
