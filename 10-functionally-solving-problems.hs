import Data.List  


-- 
-- Reverse Polish notation calculator
--

solveRPN :: (Floating a, Read a, Num a) => String -> a

solveRPN = head . foldl f [] . words
    where
        f :: (Floating a, Read a, Num a) => [a] -> String -> [a]
        f (x:y:ys) "x" = (x*y) : ys
        f (x:y:ys) "+" = (x+y) : ys
        f (x:y:ys) "-" = (x-y) : ys
        f (x:y:ys) "/" = (x/y) : ys
        f (x:y:ys) "^" = (x ** y) : ys
        f xs "sum" = [sum xs]
        f xs numberString = read numberString : xs
--
--
--Heathrow to London
--
--

data Node = Node Road (Maybe Road)  
data Road = Road Int Node  

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]  

data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]  

roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)  

optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath  

groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)  
  
main = do  
    contents <- getContents  
    let threes = groupsOf 3 (map read $ lines contents)  
        roadSystem = map (\[a,b,c] -> Section a b c) threes  
        path = optimalPath roadSystem  
        pathString = concat $ map (show . fst) path  
        pathPrice = sum $ map snd path  
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathPrice 

