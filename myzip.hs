module MyZip where

myzip :: [x] -> [y] -> [(x,y)]
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys   --If this pattern matches, it means that both the lists have at least one element
myzip _ _ = []                              --This pattern is mathched only if the first doesnt match, and this means that at least one of the two lists is empty. So we cut off other values of the non empty list.
