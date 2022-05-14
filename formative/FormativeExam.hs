module FormativeExam where

-- Question 1 (i)

allTest :: Bool
allTest =
    allSoft3 "" == True &&
    allSoft3 " " == False &&
    allSoft3 "oft3w" == True &&
    allSoft3 "ofT3w" == False &&
    allSoft3 "free" == True &&
    allSoft3 "Software3" == True

allSoft3 :: [Char] -> Bool
allSoft3 = all (`elem` "Software3")

-- Question 1 (ii)

testThreeq :: Bool
testThreeq =
    three4th 0 == 0.0 &&
    three4th 1 == 0.75 &&
    three4th 8.0 == 6.0 &&
    three4th 4.8 == 3.5999999999999996 &&
    three4th (-8) == -6.0
    
three4th :: Fractional a => a -> a
three4th = (*3) . (/4)

-- Question 1 (iii)

testSum :: Bool
testSum =
    sqSum [] == 0 &&
    sqSum [1, 3, 4] == 26 &&
    sqSum [1, 3, 4.0] == 26.0 &&
    sqSum [1, 3, 4.2] == 27.64 &&
    sqSum [1.0, 3.0, 4.0] == 26.0 &&
    sqSum [1/2, 3/2, 4.0] == 18.5 &&
    sqSum[-2, 3.0, 0.2, -1] == 14.04
    
sqSum :: Num ab => [ab] -> ab
sqSum = sum . map (^2)

-- Question 1 (iv)

testsame2other :: Bool
testsame2other =
    (same2other "" == False) &&
    (same2other "a" == False) &&
    (same2other [2] == False) &&
    (same2other "aaa" == True) &&
    (same2other [8, 8, 8] == True) &&
    (same2other [2, 2, 3, 4] == False) &&
    (same2other [2, 2, 3, 4, 2] == True) &&
    (same2other "aatdaya" == True) &&
    (same2other "aatdgyb" == False) &&
    (same2other [8, 8, 3, 8, 3, 6, 8, 12] == True)
    
same2other :: Eq a => [a] -> Bool
same2other (x:xs:xss) = x==xs && x `elem` xss
same2other (x:[])     = False
same2other []         = False

-- Question 1 (v)

testVowels :: Bool
testVowels =
    justVowels "Hello World!" == "eoo" &&
    justVowels "Aaron562qe" == "Aaoe" &&
    justVowels "sof3isGREATsoenjOY" == "oiEAoeO" &&
    justVowels "numberPLATE2021" == "ueAE"
    
justVowels :: String -> String
justVowels xs = [x | x <- xs, x `elem` "aeiouAEIOU"]

-- Question 1 (vi)

testRev :: Bool
testRev =
    revAllLower "" == "" &&
    revAllLower "!reTupmoC" == "computer!" &&
    revAllLower "Software3" == "3erawtfos" &&
    revAllLower "Software3!" == "!3erawtfos" &&
    (revAllLower $ revAllLower "Software3!") == "software3!"
    
revAllLower :: String -> String
revAllLower str = reverse [toLower x | x <- str]
    where
        toLower c | c `elem` ['A'..'Z'] = toEnum $ 32 + fromEnum c
                  | otherwise = c
                       
-- Question 1 (vii)

testfindPlurals :: Bool
testfindPlurals =
    (findPlurals "" == "") &&
    (findPlurals "THE1SOF1" == "1") &&
    (findPlurals "accommodation" == "acmo") &&
    (findPlurals "Accommodation" == "cmo") &&
    (findPlurals "THE2SOF2SYS1DAT1HCI1" == "12HST") &&
    (findPlurals [1, 3, 4, 2, 3, 5, 7, 1, 9, 3] == [1,3]) &&
    (findPlurals [1, 3, 4, 2, 3, 5, 7, 1, 9, 5] == [1,3,5]) &&
    (findPlurals [1, 5, 4, 2, 3, 5, 7, 1, 9, 3] == [1,3,5])
    
findPlurals :: Ord a => [a] -> [a]
findPlurals xs = [x | x <- sort xs, count xs x >= 2]
    where
        sort ys = foldr insert [] ys
        count zs z = length [f | f <- zs, f==z]
        insert a list | count list a == 0 = t ++ a : ts
                      | otherwise = list
            where (t,ts) = break (>a) list
            
-- Question 1 (viii)

data Course = NICE | EASY | SOFT | HARD | HALF | FULL deriving (Show, Eq)

data Student = Student SName Age College CMark
data College = Halifax | James | Langwith deriving (Show, Eq)

type SName = String
type Mark = Int
type Age = Int
type CMark = [(Course, Double)]
benWalker, jBond, yWu, kSong, mGove :: Student
benWalker = Student "Ben Walker" 19 Halifax [(SOFT, 62), (EASY, 42), (FULL, 62)]
jBond = Student "James Bond" 21 Halifax [(SOFT, 42), (EASY, 42)]
mGove = Student "Mike Gove" 21 Halifax [(SOFT, 22), (EASY, 42)]
yWu = Student "Yang Wu" 18 Halifax [(SOFT, 22)]
kSong = Student "Kelly Song" 22 Halifax []

testPrereqs :: Bool
testPrereqs =
    (checkPrereqs benWalker == False) &&
    (checkPrereqs jBond == True) &&
    (checkPrereqs yWu == True) &&
    (checkPrereqs mGove == False) &&
    (checkPrereqs kSong == True)
    
checkPrereqs :: Student -> Bool
checkPrereqs (Student _ _ _ courses) 
    | FULL `elem` courseNames = passed SOFT && passed HALF
    | EASY `elem` courseNames = passed SOFT
    | HARD `elem` courseNames = passed EASY && passed NICE
    | otherwise               = True
    where
        courseNames = map (fst) courses
        passed :: Course -> Bool
        passed cor | cor `elem` courseNames = head [mark | (name, mark) <- courses, name==cor] >= 40
                   | otherwise = False
                             
-- Question 1 (ix)

testND :: Bool
testND =
    numDiff "soft" == 1 &&
    numDiff "soft2" == 2 &&
    numDiff "soft3" == -2 &&
    numDiff "char27481" == 56 &&
    numDiff "3to15is117" == -17 &&
    numDiff "some2743367numbers" == 28

numDiff :: String -> Int
numDiff str = product even - sum odd
    where 
        odd = [read (x:"") :: Int | x <- str, x `elem` "13579"]
        even = [read (x:"") :: Int | x <- str, x `elem` "02468"]
        
-- Question 1 (x)

data CSStudent = CSStudent {sid :: SID, sname :: SName, stage :: Stage} deriving (Ord, Eq, Show)

type SID = Int

data Stage = One | Two | Three | Four deriving (Ord, Eq, Show)

data DBTreeO a = DBEmp Int | DBLeafO Int [a] | DBNodeO Int [a] [DBTreeO a] deriving (Eq, Show)

csStud, students, csYork, dbThree, stdTest :: DBTreeO CSStudent
students = DBLeafO 4 [CSStudent {sid = 2, sname = "Mark Foster", stage = One}, CSStudent {sid = 3, sname = "Juli Smith", stage = Two}]
stdTest = DBLeafO 4 [CSStudent {sid = 1, sname = "Jane Hay", stage = One}, CSStudent {sid = 3, sname = "Mat Bell", stage = Three}]
csStud = DBEmp 4
csYork = DBLeafO 3 [CSStudent {sid = 2, sname = "Mark Foster", stage = One}, CSStudent {sid = 3, sname = "Juli Smith", stage = Two}]
dbThree = DBNodeO 3 [CSStudent {sid = 3, sname = "Kriss Wells", stage = One}] [DBLeafO 3 [CSStudent {sid = 2, sname = "Sally Hodge", stage = Two}], DBLeafO 3 [CSStudent {sid = 7, sname = "Greg Ovett", stage = Two}, CSStudent {sid = 20, sname = "Ann Webb", stage = Four}]]

-- Question 1 (ix) (a)

testcsFind :: Bool
testcsFind =
    csFind students 1 == False &&
    csFind students 2 == True &&
    csFind students 3 == True &&
    csFind students 0 == False &&
    csFind students (-13) == False
    
csFind :: DBTreeO CSStudent -> SID -> Bool
-- csFind (DBLeafO _ [CSStudent sid sname stage]) = undefined 
csFind = undefined


