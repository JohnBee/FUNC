import Prelude hiding (take, drop, zipWith)
import Parse
import Data.Char (isAlpha)
-- take drop
take, drop :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x : (take (n-1) xs)

drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = (drop (n-1) xs)

-- positions
positions :: Eq a => [a] -> a -> [Int]
positions [] _ = []
positions (x:xs) y = findPos (x:xs) y 0
  where
    findPos [] y _ = []
    findPos (x:xs) y n = if x == y
                        then n : (findPos xs y (n+1))
                        else (findPos xs y (n+1))

-- duplicates
duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates xs = toSet (getDupes xs)
  where
    getDupes [] = []
    getDupes (x:xs) = if (x `elem` xs)
                  then x : (getDupes (xs))
                  else (getDupes (xs))
    toSet [] = []
    toSet (x:xs) = if (x `elem` xs)
                   then (toSet xs)
                   else x : (toSet xs)


-- sort
sort :: Ord a => [a] -> [a]
sort [] = []
sort [a] = [a]
sort [a, b] = if (a <= b)
              then [a, b]
              else [b, a]
sort xs = if xs == (singleSort xs)
          then xs
          else sort (singleSort xs)
    where
      singleSort (x:y:xs) = if (x <= y)
                            then x : (sort (y:xs))
                            else (sort [x,y]) ++ (sort xs)

-- zipWith
zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith f _ [] = []
zipWith f [] _ = []
zipWith f (x:xs) (y:ys) = (f x y) : (zipWith f xs ys)

-- Show Matrix & Transpose
data Mat a = Mat [[a]]
instance (Show a) => Show (Mat a) where
     show (Mat x) = unlines $ map (unwords . map show) x

transpose :: (Eq a) => Mat a -> Mat a
transpose (Mat []) = (Mat [])
transpose (Mat xs) = Mat (partTransp xs)
  where
    partTransp ([]:_) = []
    partTransp xs = (map head xs) : (partTransp (map tail xs))

-- Triangle
data Tri a = Tri [[a]]
instance (Show a) => Show (Tri a) where
  show (Tri x) =  unlines $ (padTriangle ((length $ (map (unwords . map show) x)) - 1) (map (unwords . map show) x))
padTriangle :: Int -> [String] -> [String]
padTriangle _ [] = []
padTriangle 0 (x:xs) = [x]
padTriangle n (x:xs) = (padding n x) : (padTriangle (n-1) xs)
  where
    padding i s = concat (replicate i " ") ++ s

trol :: Tri a -> Tri a
trol (Tri a) = Tri (f a)
  where
    f [] = []
    f (x:xs) = (f $ map tail xs) ++ [(map head (x:xs))] 

tror :: Tri a -> Tri a
tror (Tri a) = Tri (f a)
  where
    f [] = []
    f (x:xs) = (f $ map init xs) ++ [reverse (map last (x:xs))] 


sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = (sublists xs) ++ (map (x:) (sublists xs))

prefixes, suffixes :: [a] -> [[a]]
prefixes [] = []
prefixes xs = [xs] ++ prefixes (init xs)

suffixes [] = []
suffixes xs = [xs] ++ suffixes (tail xs)

segments :: [a] -> [[a]]
segments [] = []
segments (x:xs) = concat $ map suffixes (prefixes (x:xs))

parts :: [a] -> [[[a]]]
parts [] = []
parts [x] = [[[x]]]
parts (x:xs) = [p' | p@(ys:etc) <- parts xs, p' <- [[x]:p, (x:ys):etc]] 

--parser combinators
data Prog = Prog [Eqn]
data Eqn = Eqn Name [Pat] Exp
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp
data Pat = PNil | PVar Name | PCons Name Name deriving (Show)
type Name = String

name :: Parser Name
name = many1 alphas

alphas :: Parser Char
alphas = sat isAlpha

pat :: Parser Pat
pat = PNil ... string "[]" .|. PVar .:. name .|. PCons  .:. lname ..* char ':' .*. rname
  where 
    lname = (char '(' *.. name)
    rname = (name ..* char ')')
