import Text.Read (Lexeme(Number))
main :: IO ()
main = do
    print (recmax [1,2,3])
    print (headTest [5,6,7])
    print (recTake 2 [1,4,5,67])

fstNEvensB n = if n<=0 then [] else fstNEvensB (n-1) ++ [2*(n- 1)]

recmax [] = error "Cannot find max of empty list"
recmax [x] = x
recmax (x:xs) = max x (recmax xs)

headTest [] = []
headTest (x:xs) = [x]

recTake n _
    | n <= 0 = []
recTake _ [] = []
recTake n (x:xs) = x : recTake (n-1) xs

findPlurals :: Ord a => [a] -> [a]
findPlurals xs = [x | x <- sort xs, count xs x >= 2]
    where
        sort ys = foldr insert [] ys
        count zs z = length [f | f <- zs, f==z]
        insert a list | count list a == 0 = t ++ a : ts
                      | otherwise = list
            where (t,ts) = break (>a) list


-- Katie Mason
findPlurals' :: Ord a => [a] -> [a]
findPlurals' = isort . filterit
    where 
        filterit [] = []
        filterit (x:xs) | x `elem` xs = filterit xs
                        | otherwise = x : filterit xs
        insert x = insx
            where
                insx [] = [x]
                insx ys@(z:zs) | x <= z    = x : ys
                               | otherwise = z : insx zs
        isort :: Ord a => [a] -> [a]
        isort = foldr insert []


data CS1 = Student {name :: String, the1, sof1, sys1 :: Int} deriving (Show)

the1Mk :: [CS1] -> [Int]
the1Mk = map the1

the1Mk' :: [CS1] -> [Int]
the1Mk' students = [the1 | (Student _ the1 _ _) <- students]


cmbList :: [a] -> [a] -> [a]
cmbList xs ys = concat (zipWith (\ x y -> [y,x]) xs ys)

weirdFunc :: String -> String 
weirdFunc xs = calcRow xs
    where
        initLen, width, rows :: Int
        initLen = length xs
        width = floor $ sqrt $ fromIntegral initLen
        rows  =  ceiling (fromIntegral initLen / fromIntegral width)
        newXs = xs ++ replicate (rows * width - initLen) ' '
        calcRow []  = []
        calcRow xss = j ++ "\n" ++ calcRow k
            where
                (j,k) = splitAt width xss

