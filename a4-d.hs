-- a4.hs

data Token = Num Double | OpUnary String | OpBinary String | OpManip String | Err String

-- shows the tokens
instance Show Token where
    show (Num n)       = (show n)
    show (OpUnary ou)  = ou
    show (OpBinary ob) = ob
    show (OpManip om)  = om
    show (Err e)       = e

-- adds elem to stack
push :: Token -> [Token] -> [Token]
push x stack = [x] ++ stack

-- returns first elem of stack
pop :: [Token] -> Token
pop stack = head stack

-- returns rest of stack
rest :: [Token] -> [Token]
rest []    = [Err "empty stack!"]
rest stack = tail stack

-- checks if string is a double
isDouble :: String -> Bool
isDouble x = (not (null (reads x :: [(Double,String)]))) && ((snd (head (reads x :: [(Double,String)])))=="")

-- checks if string is a unary op
isOpUnary :: String -> Bool
isOpUnary x = x == "inc" || x == "dec" || x == "sqrt" || x == "sin" || x == "cos" || x == "inv" || x == "dup" 

-- checks if string is binary op
isOpBinary :: String -> Bool
isOpBinary x = x == "+" || x == "*" || x == "-" || x == "/" ||  x == "swap" 

-- checks if string is manip op
isOpManip :: String -> Bool
isOpManip x = x == "+all" || x == "*all" || x == "clear" || x == "pop"

-- converts string to a token
stringToToken :: String -> Token
stringToToken x | isDouble x   = Num (fst (head (reads x :: [(Double, String)])))
                | isOpUnary x  = OpUnary x
                | isOpBinary x = OpBinary x
                | isOpManip x  = OpManip x
                | otherwise    = Err "not a valid token!"

-- converts list of tokens into list of doubles
tokenToDouble :: [Token] -> [Double]
tokenToDouble []              = []
tokenToDouble ((Num n):xs) = n : tokenToDouble xs

-- converts token to a string
tokenToString :: Token -> String
tokenToString (Err x)        = x
tokenToString (OpUnary opu)  = opu
tokenToString (OpBinary opb) = opb
tokenToString (OpManip opm)  = opm
tokenToString (Num n)        = show n

-- converts string to a list of tokens
stringToList :: String -> [Token]
stringToList x = map stringToToken (words x)

-- all the calculations for a unary operator
applyUnary :: [Token] -> Token -> [Token]
applyUnary ((Num n):stack) (OpUnary opu) | opu == "inc"   = (Num (n + 1)):stack
                                         | opu == "dec"   = (Num (n - 1)):stack
                                         | opu == "sqrt"  = (Num (sqrt n)):stack
                                         | opu == "sin"   = (Num (sin n)):stack
                                         | opu == "cos"   = (Num (cos n)):stack
                                         | opu == "inv"   = (Num (1/n)):stack
                                         | opu == "dup"   = (Num n):(Num n):stack
                                         | otherwise      = [Err "not enough args!"]

-- all the calculations for a binary operator
applyBinary :: [Token] -> Token -> [Token]
applyBinary ((Num n1):(Num n2):stack) (OpBinary opb)   | opb == "+"    = (Num (n1 + n2)):stack
                                                       | opb == "*"    = (Num (n1 * n2)):stack
                                                       | opb == "-"    = (Num (n1 - n2)):stack
                                                       | opb == "/"    = (Num (n2 / n1)):stack
                                                       | opb == "dup"  = (Num n1):(Num n1):(Num n2):stack
                                                       | opb == "swap" = (Num n1):(Num n2):stack
                                                       | opb == "sqrt" = (Num (sqrt n1)):(Num n2):stack
                                                       | otherwise     = [Err "not enough args!"]

-- all the calculations for a manipulation operator
applyManip :: [Token] -> Token -> [Token]
applyManip stack (OpManip opm) | opm == "+all"  = [addAllTokens stack]
                               | opm == "*all"  = [multiplyAllTokens stack]
                               | opm == "pop"   = tail stack
                               | opm == "clear" = []
                               | otherwise      = [Err "something is off!"]

-- function for +all 
addAllTokens :: [Token] -> Token
addAllTokens stack = Num (foldr (+) 0 (tokenToDouble(stack)))

-- function for *all
multiplyAllTokens :: [Token] -> Token
multiplyAllTokens stack = Num (foldr (*) 1 (tokenToDouble(stack)))

-- (input, stack) 
evaluate :: ([Token], [Token]) -> [Token]
evaluate ([], [])                                       = [Err "empty stack!"]        -- 1. both input and stack are empty
evaluate ([], stack)                                    = stack                       -- 2. list is empty, stack stays the same
evaluate ((x:xs), stack) | isDouble (tokenToString x)   = evaluate (xs, push x stack) -- 3. if elem is a number then push it on stack
                         | isOpUnary (tokenToString x)  =  if (length stack < 1)      -- 4. if operator is unary, calls applyUnary
                                                            then [Err "not enough args!"]
                                                            else evaluate (xs, (applyUnary stack x))                      
                         | isOpBinary (tokenToString x) = if (length stack < 2)       -- 5. if operator is binary, calls applyBinary
                                                            then [Err "not enough args!"]
                                                            else evaluate (xs, (applyBinary stack x))
                         | isOpManip (tokenToString x)  = if (null stack)             -- 6. if operator is manipulator, calls applyManip
                                                            then [Err "empty stack!"]
                                                            else evaluate (xs, (applyManip stack x))
                         | otherwise = [Err "try again"]                              -- 7. otherwise wrong input

-- processed stack as a string
calcStack :: String -> String
calcStack x | (length x == 0) = show [Err "Stack is empty!"]
            | otherwise       = show (evaluate ((stringToList x), []))

-- first element of calcStack
calc :: String -> String
calc x | (length x == 0) = show [Err "Stack is empty!"]
       | otherwise       = show (head (evaluate ((stringToList x), []))) 