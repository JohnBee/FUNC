breadthFirst :: (a -> [a]) -> a -> [a]
breadthFirst b r  =  bf b [r]

bf :: (a -> [a]) -> [a] -> [a]
bf b []      =  []
bf b (x:xs)  =  x : bf b (xs ++ b x)
