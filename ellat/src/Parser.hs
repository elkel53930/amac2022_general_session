module Parser where

import Type

type Parser a = String -> Maybe (a, String)

ret :: a -> Parser a
ret v = \inp -> Just (v, inp)

failure :: Parser a
failure = \inp -> Nothing

item :: Parser Char
item = \inp -> case inp of
    [] -> Nothing
    (x:xs) -> Just (x, xs)

(>>>) :: Parser a -> (a -> Parser b) -> Parser b
p >>> f = \inp -> case p inp of
    Nothing -> Nothing
    Just (v, out) -> (f v) out

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
    Nothing -> q inp
    Just (v, out) -> Just (v, out)

sat :: (Char -> Bool) -> Parser Char
sat p = item >>> \x
     -> if p x then ret x else failure

isIn :: Char -> [Char] -> Bool
isIn _ [] = False
isIn t (c:cs) = if c == t then True else isIn t cs

isDigit x = isIn x ['0'..'9']
isAlpha x = isIn x (['a'..'z'] ++ ['A'..'Z'])

digit = sat isDigit
alpha = sat isAlpha
char x = sat (\y -> x==y)

string :: String -> Parser String
string [] = ret []
string (x:xs) = char x >>> \_
             -> string xs >>> \_
             -> ret ([x] ++ xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ ret []
many1 :: Parser a -> Parser [a]
many1 p = p >>> \v
       -> many p >>> \vs
       -> ret ([v] ++ vs)

isSpace x = isIn x (" \t\n")
space = sat isSpace

token :: Parser a -> Parser a
token p = (many space) >>> \_
       -> p >>> \x
       -> (many space) >>> \_
       -> ret x

identifier :: Parser String
identifier = (char '_') +++ alpha >>> \h
           -> many ((char '_') +++ alpha +++ digit) >>> \t
           -> ret ([h] ++ t)

expression :: Parser Expression
expression = literal +++ variable +++ binaryOperation

literal :: Parser Expression
literal = many1 digit >>> \x
       -> ret (Literal (read x))

variable :: Parser Expression
variable = identifier >>> \v
        -> ret (Variable v)

{-
binList :: [(String, BinaryOperator)]
binList = [
    ("+", Add), ("-", Sub), ("*", Mul), ("/", Div),
    ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge),
    ("==", Eq), ("!=", Neq),
    ("&&", And), ("||", Or)
    ]

mkBinOp :: (String, BinaryOperator) -> Parser BinaryOperator
mkBinOp (s, op) = string s >>> \_ -> ret op

binaryOperator :: Parser BinaryOperator
binaryOperator = foldl1 (+++) ps
    where ps = map mkBinOp binList
-}

binaryOperator :: Parser BinaryOperator
binaryOperator = add +++ sub +++ mul +++ div +++ le +++ lt +++ ge +++ gt +++ eq +++ neq +++ and +++ or
    where
        add = string "+" >>> \_ -> ret Add
        sub = string "-" >>> \_ -> ret Sub
        mul = string "*" >>> \_ -> ret Mul
        div = string "/" >>> \_ -> ret Div
        lt = string "<" >>> \_ -> ret Lt
        le = string "<=" >>> \_ -> ret Le
        gt = string ">" >>> \_ -> ret Gt
        ge = string ">=" >>> \_ -> ret Ge
        eq = string "==" >>> \_ -> ret Eq
        neq = string "!=" >>> \_ -> ret Neq
        and = string "&&" >>> \_ -> ret And
        or = string "||" >>> \_ -> ret Or

binaryOperation :: Parser Expression
binaryOperation = token (char '(') >>> \_
               -> token expression >>> \ls
               -> token binaryOperator >>> \op
               -> token expression >>> \rs
               -> token (char ')') >>> \_
               -> ret (BinaryOperation op ls rs)

statement :: Parser Statement
statement = Parser.sequence +++ withoutSequence

withoutSequence :: Parser Statement
withoutSequence = declareVariable +++ assign +++ if_ +++ pass +++ while

declareVariable :: Parser Statement
declareVariable = token (string "var") >>> \_
               -> token identifier >>> \v
               -> token (string ":=") >>> \_
               -> token expression >>> \init
               -> ret (DeclareVariable v init)

assign :: Parser Statement
assign = token identifier >>> \v
      -> token (string ":=") >>> \_
      -> token expression >>> \e
      -> ret (Assign v e)

if_ :: Parser Statement
if_ = token (string "if") >>> \_
   -> token (expression) >>> \cond
   -> token (string "{") >>> \_
   -> token (statement) >>> \thenS
   -> token (string "}") >>> \_
   -> token (string "else") >>> \_
   -> token (string "{") >>> \_
   -> token (statement) >>> \elseS
   -> token (string "}") >>> \_
   -> ret (If cond thenS elseS)

pass :: Parser Statement
pass = token (string "pass") >>> \_
    -> ret Pass

while :: Parser Statement
while = token (string "while") >>> \_
     -> token (expression) >>> \cond
     -> token (string "{") >>> \_
     -> token (statement) >>> \s
     -> token (string "}") >>> \_
     -> ret (While cond s)

sequence :: Parser Statement
sequence = token withoutSequence >>> \s1
        -> token (string ";") >>> \_
        -> token statement >>> \s2
        -> ret (Sequence s1 s2) 