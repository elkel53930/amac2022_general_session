data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving Show

data Person = Person String Int deriving Show -- Name and Age

data Shape = Circle Double -- Diameter​
           | Rectangle Double Double -- Width, Height​
           | Line Double Double -- Angle, Length​
           deriving Show

data IntList = LNode Int IntList
             | LNil
             deriving Show

data IntTree = TNode Int IntTree IntTree
             | TNil
             deriving Show

data Op = Sum | Sub | Mul | Div deriving Show
data Expr = Literal Int
          | BiOp Op Expr Expr
          deriving Show

add :: Int -> Int -> Int
add x y = x + y

cond :: Bool -> Int -> Int -> Int
cond condition x y =
    if condition == True then
        x
    else
        y

isHoliday :: DayOfWeek -> Bool
isHoliday Sun = True
isHoliday Mon = False
isHoliday Tue = False
isHoliday Wed = False
isHoliday Thu = False
isHoliday Fri = False
isHoliday Sat = True

isHoliday' :: DayOfWeek -> Bool
isHoliday' Sun = True
isHoliday' Sat = True
isHoliday' d = False

isHoliday'' :: DayOfWeek -> Bool
isHoliday'' Sun = True
isHoliday'' Sat = True
isHoliday'' _ = False

area :: Shape -> Double
area (Circle diameter) = radius * radius * 3.14
    where radius = diameter / 2
area (Rectangle width height) = width * height
area (Line _ _) = 0

total :: IntList -> Int
total LNil = 0
total (LNode value next) = value + (total next)

total' :: [Int] -> Int
total' [] = 0
total' (x:xs) = x + total' xs

data List a = Node a (List a)
            | Nil
            deriving Show

divine :: Int -> Int -> Maybe Int
divine x y = if y == 0 then Nothing else Just (x `div` y)

area' :: Shape -> Double
area' s = case s of
    Circle diameter -> radius * radius * 3.14
        where radius = diameter / 2
    Rectangle width height -> width * height
    Line _ _ -> 0

takeHead :: String -> Maybe Char
takeHead [] = Nothing
takeHead (s:_) = Just s

isIn :: [Char] -> Char -> Bool
isIn [] _ = False
isIn (c:cs) x = if c == x then True else isIn cs x

isDigit = isIn ['0'..'9']

takeDigit :: String -> String
takeDigit [] = ""
takeDigit (s:ss) = if isDigit s then [s] ++ takeDigit ss else takeDigit ss

(<->) :: Int -> Int -> Int
(<->) x y = if r < 0 then 0 else r
    where r = x -y

adderGenerator :: Int -> (Int -> Int)
adderGenerator x = \y -> x + y