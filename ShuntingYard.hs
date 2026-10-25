module NagyBead where

import Data.Either
import Data.Maybe
import Text.Read
import Data.Either

basicInstances = 0 -- Mágikus tesztelőnek kell ez, NE TÖRÖLD!

data Dir = InfixL | InfixR
 deriving (Show, Eq, Ord)

data Tok a = BrckOpen | BrckClose | TokLit a | TokBinOp (a -> a -> a) Char Int Dir | TokFun (a -> a) String
instance Show a => Show (Tok a) where
  show BrckOpen = "BrckOpen"
  show BrckClose = "BrckClose"
  show (TokLit a) = "TokLit " ++ show a
  show (TokBinOp _ symbol strength direction ) = "TokBinOp " ++ show symbol ++ " " ++ show strength ++ " " ++ show direction
  show (TokFun _ name) = "TokFun " ++ name
-- TokBinOp :: (a -> a -> a) -> Char -> Int -> Dir -> Tok a
instance Eq a => Eq (Tok a) where
 BrckOpen == BrckOpen = True
 BrckClose == BrckClose = True
 (TokLit a) == (TokLit b) = a == b
 (TokBinOp _ symbol1 strength1 direction1) == (TokBinOp _ symbol2 strength2 direction2) = symbol1 == symbol2 && strength1 == strength2 && direction1 == direction2
 (TokFun _ name1) == (TokFun _ name2) = name1 == name2
 _ == _ = False


data ShuntingYardError = OperatorOrClosingParenExpected | LiteralOrOpeningParenExpected | NoClosingParen | NoOpeningParen | ParseError
 deriving (Eq, Show)

type ShuntingYardResult a = Either ShuntingYardError a

type FunctionTable a = [(String, (a -> a))]

functionTable :: (RealFrac a, Floating a) => FunctionTable a 
functionTable = 
    [ ("sin", sin) 
    , ("cos", cos) 
    , ("log", log) 
    , ("exp", exp) 
    , ("sqrt", sqrt) 
    , ("round", (\x -> fromIntegral (round x :: Integer))) 
    ] 
 
tSin, tCos, tLog, tExp, tSqrt :: Floating a => Tok a 
tSin = TokFun sin "sin" 
tCos = TokFun cos "cos" 
tLog = TokFun log "log" 
tExp = TokFun exp "exp" 
tSqrt = TokFun sqrt "sqrt" 
 
tRound :: (Floating a, RealFrac a) => Tok a 
tRound = TokFun (\x -> fromIntegral (round x :: Integer)) "round"

type OperatorTable a = [(Char, (a -> a -> a, Int, Dir))]

tAdd, tMinus, tMul, tDiv, tPow :: (Floating a) => Tok a
tAdd = TokBinOp (+) '+' 6 InfixL
tMinus = TokBinOp (-) '-' 6 InfixL
tMul = TokBinOp (*) '*' 7 InfixL
tDiv = TokBinOp (/) '/' 7 InfixL
tPow = TokBinOp (**) '^' 8 InfixR

operatorTable :: (Floating a) => OperatorTable a
operatorTable =
    [ ('+', ((+), 6, InfixL))
    , ('-', ((-), 6, InfixL))
    , ('*', ((*), 7, InfixL))
    , ('/', ((/), 7, InfixL))
    , ('^', ((**), 8, InfixR))
    ]

----------------------------------------------------------------------

operatorFromChar :: OperatorTable a -> Char -> Maybe (Tok a)
operatorFromChar [] _ = Nothing
operatorFromChar ((symbol, (meaning, base, direction)) : xs) char
    | char == symbol = Just (TokBinOp meaning symbol base direction)
    | otherwise = operatorFromChar xs char

getOp :: (Floating a) => Char -> Maybe (Tok a)
getOp = operatorFromChar operatorTable

----------------------------------------------------------------------

type NothingFlag = Bool

parseTokens :: Read a => OperatorTable a -> String -> Maybe [Tok a]   
parseTokens table list = case tokenize (words list) table of
     (True, _) -> Nothing
     (_, xs) -> Just xs

tokenize :: Read a => [String] -> OperatorTable a -> (NothingFlag, [Tok a]) -- FUGGVENY CELJA: BEMENET ATALAKITASA TOKENEKKE, MUVELET SIKERESSEGENEK JELZESE                                     -- REKURZIO LEALLITASAT SZOLGALJA??
tokenize [] _ = (False, [])                                   -- HA ELFOGYOTT A BEMENET, AKKOR AZ EDDIG GYUJTOGETETT LISTA VISSZAADASA
tokenize (a@(x:xs):xss) table                               
    | x == '(' || x == ')' = case multipleBrackets a of                 -- HA AZ EGYSEG ZAROJELLEL KEZDODIK, MEG KELL NEZNI, LITERAL LESZ-E VAGY CSUPA ZAROJEL (MIT AD VISSZA A MULTIPLEBRACKETS)
         Nothing -> case literalA of                               -- HA VAN BENNE LITERAL IS, AKKOR MEGRPOBALNI TOKENIZALNI
            Nothing -> (True, [])                                       -- HA SIKERTELEN, AKKOR REKURZIO LEALLITASA
            _ ->  let (something, zs) = tokenize xss table in (something, fromJust literalA : zs)          -- HA SIKERES, AKKOR HOZZAADJUK A VEGERDEMENY TOKENLISTAHOZ ES REKURZIVAM HIVUNK             
         bracketList -> let (something, zs) = tokenize xss table in (something, fromJust bracketList ++ zs) -- HA CSUPA ZAROJEL, AKKOR EGYESEVEL TOKENNE ALAKITVA FELFUZNI ES REKURZIV HIVAS
    | xs == [] = case opX of                   -- HA AZ EGYSEG EGYELEMU, AKKOR LEHET, HOGY OPERATOR, TEHAT ERRE TESZTELUNK
         Nothing -> case literalA of                                -- HA NEM OPERATOR, AKKOR MEGNEZZUK, NEM EGYJEGYU LITERAL-E
             Nothing -> (True, [])                                  -- HA NEM IS ZAROJEL, NEM IS OPERATOR, NEM IS LITERAL, AKKOR REKURZIO LEALLITASA
             _ -> let (something, zs) = tokenize xss table in (something, fromJust literalA : zs)          -- HA OPERATORRA NEM, DE LITERALLA ALAKITHATO, AKKOR A LITERALT FELFUZZUK ES REKURZIV MEGHIVAS
         _ -> let (something, zs) = tokenize xss table in (something, fromJust opX : zs) -- HA OPERATORRA ALAKITHATO, AKKOR AZ OPERATORT FELFUZZUK ES REKURZIV HIVAS
    | otherwise = case literalA of                                  -- HA NEM KEZDODIK ZAROJELLEL, ES NEM IS EGYJEGYU, CSAK LITERAL LEHET. 
         Nothing -> (True, [])                                      -- HA AZ ATALAKITAS SIKERTELEN, REKURZIO LEALLITASA
         _ -> let (something, zs) = tokenize xss table in (something, fromJust literalA : zs)          --HA SIKERES, A LITERALT FELFUZZUK ES REKURZIV HIVAS
    where   
        literalA = makeLiteral a
        opX = operatorFromChar table x

multipleBrackets :: Read a => String -> Maybe [(Tok a)]                 -- FUGGVENY CELJA: MEGNEZNI, HOGY CSUPA ZAROJELBOL ALL-E EGY EGYSEG. HA IGEN, ATALAKITAS
multipleBrackets [] = Just []                                           -- REKURZIO ALAPESETE
multipleBrackets a@(x : xs)
  | x == '(' = case multipleBrackets xs of                          -- NYITO FELFUZESE
      Just brackets -> Just (BrckOpen : brackets)
      Nothing -> Nothing
  | x == ')' = case multipleBrackets xs of                          -- ZARO FELFUZESE
      Just brackets -> Just (BrckClose : brackets)
      Nothing -> Nothing
  | otherwise = Nothing                                             -- JELZES A TOKENIZE-NAK, HOGY ITT LITERAL IS LEHET, NEM ZAROJELKENT KEZELENDO

makeLiteral :: Read a => String -> Maybe (Tok a)                    -- READMAYBE KISZERVEZESE
makeLiteral xs = fmap TokLit (readMaybe xs)                         --FMAP: TokLit (Just xs)-bol Just (TokLit xs)-t csinal

parse :: String -> Maybe [Tok Double]
parse = parseTokens operatorTable

-------------------------------------------------------------------------------------------

shuntingYardBasic :: [Tok a] -> ([a], [Tok a])                      -- FUGGVENY CELJA: TOKENLISTA SZETVALASZTASA KIERTEKELES UTAN MEGMARADT OPERATOROKRA + ZAROJELEKRE ES LITERALOKRA
shuntingYardBasic xs = collector xs [] [] where
    collector :: [Tok a] -> [a] -> [Tok a] -> ([a], [Tok a])            -- FUGGVENY CELJA: SZETVALOGATNI A LITERALOKAT ES OPERATOROKAT STACK-SZERUEN
    collector (TokLit v : ys) literalList operatorList = collector ys (v : literalList) operatorList --HA A BEMENET ELEJEN LITERAL VAN, AKKOR HOZZAADJUK A LITERALLISTAHOZ ES MEGYUNK TOVABB
    collector (BrckClose : ys) literalList operatorList = let (remainLits, remainOps) = evalInside (literalList, operatorList) in collector ys (remainLits) (remainOps) -- HA ZARO ZAROJEL VAN, AKKOR KIERTEKELEST VEGZUNK
    collector (y : ys) literalList operatorList = collector ys literalList (y : operatorList) -- HA NEM LITERAL VAN A BEMENET ELEJEN, AKKOR OPERATORLISTAHOZ ADJUK ES MEGYUNK TOVABB
    collector [] literalList operatorList = (literalList, operatorList)     -- HA ELFOGY A BEMENET, AKKOR VISSZAADJUK A SZETVALOGATOTT LITERALOKAT ES OPERATOROKAT

evalInside :: ([a], [Tok a]) -> ([a], [Tok a])      -- FUGGVENY CELJA: NYITO ZAROJEL ELERESEIG KIERTEKELES VEGZESE, ES MARADEK OPERATOROK ILLETVE UPDATELT LITERALLISTA VISSZAADASA
evalInside (c@(x : xs), (BrckOpen : ys)) = (c, ys)  -- NYITO ZAROJELNEL AZ AKTUALIS LITERALLISTA ES A ZAROJEL UTANI OPERATORLISTA VISSZAADASA
evalInside ((x : xs: xss), (TokBinOp f _ _ _ : ys)) = evalInside ((f xs x) : xss, ys)   --MIVEL LEGALABB KET LITERAL KELL AZ INFIX OPERATOROKHOZ, IGY LITERALLISTA ELSO KET ELEMENEK KIERTEKELESE AZ ELSO OPERATORRAL, REKURZIV MEGHIVAS
evalInside ((x:xs), (TokFun f _) : ys) = evalInside ((f x): xs, ys)
evalInside ([],(BrckOpen : ys)) = ([], ys)          -- HA ELFOGY A LITERALLISTANK, ES NYITO ZAROJEL VAN, AKKOR AZ URES LISTAT KULDJUK VISSZA A MARADEK OPERATOROKKAL

parseAndEval :: (String -> Maybe [Tok a]) -> ([Tok a] -> ([a], [Tok a])) -> String -> Maybe ([a], [Tok a])
parseAndEval parse eval input = maybe Nothing (Just . eval) (parse input)

syNoEval :: String -> Maybe ([Double], [Tok Double])
syNoEval = parseAndEval parse shuntingYardBasic

syEvalBasic :: String -> Maybe ([Double], [Tok Double])
syEvalBasic = parseAndEval parse (\t -> shuntingYardBasic $ BrckOpen : (t ++ [BrckClose]))

---------------------------------------------------------------------------------

shuntingYardPrecedence :: [Tok a] -> ([a], [Tok a])  
shuntingYardPrecedence xs = collector xs [] [] where
    collector :: [Tok a] -> [a] -> [Tok a] -> ([a], [Tok a])
    collector (BrckClose : ys) literalList operatorList = let (values, remainOps) = evalInside (literalList, operatorList) in collector ys values remainOps
    collector (TokLit v : ys) literalList operatorList = collector ys (v : literalList) operatorList
    collector (y : ys) literalList [] = collector ys literalList [y]
    collector (newOp@(TokBinOp _ _ strength1 dir) : ys) literalList operatorList = let (values, remainOps) = eval literalList operatorList strength1 dir in collector ys values (newOp: remainOps)
    collector (y : ys) literalList operatorList = collector ys literalList (y : operatorList) -- nyitó zárójel és function
    collector [] literalList operatorList = (literalList, operatorList)

eval :: [a] -> [Tok a] -> Int -> Dir -> ([a], [Tok a])
eval b@(x : xs : xss) c@(TokBinOp f _ strength1 _ : ys) strength dir
    | dir == InfixL && strength <= strength1 = eval ((f xs x) : xss) ys strength dir
    | dir == InfixR && strength < strength1 = eval ((f xs x) : xss) ys strength dir
    | otherwise = (b, c)
eval (x : xs) (TokFun f _ : ys) strength dir = eval ((f x) : xs) ys strength dir 
eval xs ys@(BrckOpen : _) _ _ = (xs, ys)
eval xs [] _ _ = (xs, [])
    
syEvalPrecedence :: String -> Maybe ([Double], [Tok Double])
syEvalPrecedence = parseAndEval parse (\t -> shuntingYardPrecedence $ BrckOpen : (t ++ [BrckClose]))

-------------------------------------------------------------------------------------------

parseWithFunctions :: Read a => OperatorTable a -> FunctionTable a -> String -> Maybe [Tok a]  
parseWithFunctions opTable funcTable list = case tokenize' (words list) opTable funcTable of
     (True, _) -> Nothing
     (_, xs) -> Just xs

tokenize' :: Read a => [String] -> OperatorTable a -> FunctionTable a -> (NothingFlag, [Tok a]) -- FUGGVENY CELJA: BEMENET ATALAKITASA TOKENEKKE, MUVELET SIKERESSEGENEK JELZESE                                     -- REKURZIO LEALLITASAT SZOLGALJA??
tokenize' [] _ _ = (False, [])                                   -- HA ELFOGYOTT A BEMENET, AKKOR AZ EDDIG GYUJTOGETETT LISTA VISSZAADASA
tokenize' (a@(x:xs):xss) opTable funcTable                          
    | x == '(' || x == ')' = case multipleBrackets a of                 -- HA AZ EGYSEG ZAROJELLEL KEZDODIK, MEG KELL NEZNI, LITERAL LESZ-E VAGY CSUPA ZAROJEL (MIT AD VISSZA A MULTIPLEBRACKETS)
         Nothing -> case literalA of                               -- HA VAN BENNE LITERAL IS, AKKOR MEGRPOBALNI TOKENIZALNI
            Nothing -> (True, [])                                       -- HA SIKERTELEN, AKKOR REKURZIO LEALLITASA
            _ ->  let (something, zs) = tokenize' xss opTable funcTable in (something, fromJust literalA : zs)          -- HA SIKERES, AKKOR HOZZAADJUK A VEGERDEMENY TOKENLISTAHOZ ES REKURZIVAM HIVUNK             
         bracketList -> let (something, zs) = tokenize' xss opTable funcTable in (something, fromJust bracketList ++ zs) -- HA CSUPA ZAROJEL, AKKOR EGYESEVEL TOKENNE ALAKITVA FELFUZNI ES REKURZIV HIVAS
    | xs == [] = case opX of                   -- HA AZ EGYSEG EGYELEMU, AKKOR LEHET, HOGY OPERATOR, TEHAT ERRE TESZTELUNK
         Nothing -> case funcA of
            Nothing-> case literalA of                                -- HA NEM OPERATOR, AKKOR MEGNEZZUK, NEM EGYJEGYU LITERAL-E
                Nothing -> (True, [])                                  -- HA NEM IS ZAROJEL, NEM IS OPERATOR, NEM IS LITERAL, AKKOR REKURZIO LEALLITASA
                _ -> let (something, zs) = tokenize' xss opTable funcTable in (something, fromJust literalA : zs)
            _ -> let (something, zs) = tokenize' xss opTable funcTable in (something, fromJust funcA : zs)
         _ -> let (something, zs) = tokenize' xss opTable funcTable in (something, fromJust opX : zs)
    | otherwise = case funcA of                                  -- HA NEM KEZDODIK ZAROJELLEL, ES NEM IS EGYJEGYU, CSAK LITERAL LEHET. 
         Nothing -> case literalA of
            Nothing -> (True, [])
            _ -> let (something, zs) = tokenize' xss opTable funcTable in (something, fromJust literalA : zs)                                    -- HA AZ ATALAKITAS SIKERTELEN, REKURZIO LEALLITASA
         _ -> let (something, zs) = tokenize' xss opTable funcTable in (something, fromJust funcA : zs)       --HA SIKERES, A LITERALT FELFUZZUK ES REKURZIV HIVAS
    where   
        literalA = makeLiteral a
        funcA = functionFromString funcTable a
        opX = operatorFromChar opTable x

functionFromString :: FunctionTable a -> String -> Maybe (Tok a)
functionFromString [] _ = Nothing
functionFromString ((name, function) : xs) functionInQuestion
    | name == functionInQuestion = Just (TokFun function name)
    | otherwise = functionFromString xs functionInQuestion

shuntingYardWithFunctions :: [Tok a] -> ([a], [Tok a])
shuntingYardWithFunctions xs = shuntingYardPrecedence xs

syFun :: String -> Maybe ([Double], [Tok Double]) 
syFun = parseAndEval 
  (parseWithFunctions operatorTable functionTable) 
  (\t -> shuntingYardWithFunctions $ BrckOpen : (t ++ [BrckClose]))


-------------------------

parseSafe :: Read a => OperatorTable a -> String -> ShuntingYardResult [Tok a]  
parseSafe table list = case tokenizeSafe (words list) table of
     (True, _) -> Left ParseError
     (_, xs) -> Right xs
    where
    tokenizeSafe :: Read a => [String] -> OperatorTable a -> (NothingFlag, [Tok a])
    tokenizeSafe [] _ = (False, [])                                   
    tokenizeSafe (a@(x:xs):xss) table                               
        | x == '(' || x == ')' = case multipleBrackets a of
            Nothing -> case literalA of                               
                Nothing -> (True, [])                                       
                _ ->  let (something, zs) = tokenizeSafe xss table in (something, fromJust literalA : zs)          
            bracketList -> let (something, zs) = tokenizeSafe xss table in (something, fromJust bracketList ++ zs)
        | xs == [] = case operatorFromChar table x of                   
            Nothing -> case literalA of                                
                Nothing -> (True, [])                                  
                _ -> let (something, zs) = tokenizeSafe xss table in (something, fromJust literalA : zs)
            _ -> let (something, zs) = tokenizeSafe xss table in (something, fromJust opX : zs)
        | otherwise = case literalA of                                
            Nothing -> (True, [])                                      
            _ -> let (something, zs) = tokenizeSafe xss table in (something, fromJust literalA : zs)         
        where       
            literalA = makeLiteral a
            opX = operatorFromChar table x

shuntingYardSafe :: [Tok a] -> ShuntingYardResult ([a], [Tok a])
shuntingYardSafe xs = (case collector xs [] [] of
    result@(Right (_ , ys)) -> case isThereAnOpen ys of
        True -> Left NoClosingParen
        _ -> result
    error -> error)
    where
    collector :: [Tok a] -> [a] -> [Tok a] ->  ShuntingYardResult ([a], [Tok a])
    collector (TokFun _ _ : ys : yss) _ _
        | not (isLit ys) && not (isOpen ys) = Left LiteralOrOpeningParenExpected
    collector (y : ys : yss) _ _
        | (isClose y || isLit y) && not (isOp ys) && not (isClose ys) = Left OperatorOrClosingParenExpected
    collector (y : ys : yss) _ _
        | (isOp y || isOpen y) && not (isOpen ys) && not (isLit ys) && not (isFun ys) = Left LiteralOrOpeningParenExpected
    collector (BrckClose : ys) literalList operatorList = case evalInsideSafe (literalList, operatorList) of
        Left NoOpeningParen -> Left NoOpeningParen
        _ -> let (values, remainOps) = evalInside (literalList, operatorList) in collector ys values remainOps
    collector (TokLit v : ys) literalList operatorList = collector ys (v : literalList) operatorList
    collector (y : ys) literalList [] = collector ys literalList [y]
    collector (newOp@(TokBinOp _ _ strength1 dir) : ys) literalList operatorList = let (values, remainOps) = eval literalList operatorList strength1 dir in collector ys values (newOp: remainOps)
    collector (y : ys) literalList operatorList = collector ys literalList (y : operatorList) 
    collector [] literalList operatorList = Right (literalList, operatorList)

    evalInsideSafe :: ([a], [Tok a]) -> ShuntingYardResult ([a], [Tok a])
    evalInsideSafe (_, []) = Left NoOpeningParen
    evalInsideSafe (xs, (BrckOpen : ys)) = Right (xs, ys)
    evalInsideSafe ((x : xs: xss), (TokBinOp f _ _ _ : ys)) = evalInsideSafe ((f xs x) : xss, ys)
    evalInsideSafe ((x:xs), (TokFun f _) : ys) = evalInsideSafe ((f x) : xs, ys)

    isThereAnOpen :: [Tok a] -> Bool
    isThereAnOpen (BrckOpen : xs) = True
    --isThereAnOpen (x : xs) = isThereAnOpen xs
    isThereAnOpen _ = False

    isOp :: Tok a -> Bool
    isOp (TokBinOp _ _ _ _) = True
    isOp _ = False

    isFun :: Tok a -> Bool
    isFun (TokFun _ _) = True
    isFun _ = False

    isLit :: Tok a -> Bool
    isLit (TokLit _) = True
    isLit _ = False

    isClose :: Tok a -> Bool
    isClose BrckClose = True
    isClose _ = False

    isOpen :: Tok a -> Bool
    isOpen BrckOpen = True
    isOpen _ = False

eqError = 0 -- Ez a tesztelőnek szükséges! 
 
parseAndEvalSafe :: 
    (String -> ShuntingYardResult [Tok a]) -> 
     ([Tok a] -> ShuntingYardResult ([a], [Tok a])) -> 
     String -> ShuntingYardResult ([a], [Tok a]) 
parseAndEvalSafe parse eval input = either Left eval (parse input) 

sySafe :: String -> ShuntingYardResult ([Double], [Tok Double]) 
sySafe = parseAndEvalSafe 
  (parseSafe operatorTable) 
  (\ts -> shuntingYardSafe (BrckOpen : ts ++ [BrckClose]))

----------------------------------

parseComplete :: Read a => OperatorTable a -> FunctionTable a -> String -> ShuntingYardResult [Tok a]   
parseComplete opTable funcTable list = case tokenizeComplete (words list) opTable funcTable of
     (True, _) -> Left ParseError
     (_, xs) -> Right xs
    where
    tokenizeComplete :: Read a => [String] -> OperatorTable a -> FunctionTable a -> (NothingFlag, [Tok a])
    tokenizeComplete [] _ _ = (False, [])                                    
    tokenizeComplete (a@(x:xs):xss) opTable funcTable                          
        | x == '(' || x == ')' = case multipleBrackets a of                   
            Nothing -> case literalA of                                
                Nothing -> (True, [])                                  
                _ ->  let (something, zs) = tokenizeComplete xss opTable funcTable in (something, fromJust literalA : zs)                 
            bracketList -> let (something, zs) = tokenizeComplete xss opTable funcTable in (something, fromJust bracketList ++ zs)  
        | xs == [] = case opX of                   
            Nothing -> case funcA of
                Nothing-> case literalA of                                
                    Nothing -> (True, [])                                  
                    _ -> let (something, zs) = tokenizeComplete xss opTable funcTable in (something, fromJust literalA : zs)
                _ -> let (something, zs) = tokenizeComplete xss opTable funcTable in (something, fromJust funcA : zs)
            _ -> let (something, zs) = tokenizeComplete xss opTable funcTable in (something, fromJust opX : zs)
        | otherwise = case funcA of                                   
            Nothing -> case literalA of
                Nothing -> (True, [])
                _ -> let (something, zs) = tokenizeComplete xss opTable funcTable in (something, fromJust literalA : zs)                                     
            _ -> let (something, zs) = tokenizeComplete xss opTable funcTable in (something, fromJust funcA : zs)
        where   
            literalA = makeLiteral a
            funcA = functionFromString funcTable a
            opX = operatorFromChar opTable x

shuntingYardComplete :: [Tok a] -> ShuntingYardResult ([a], [Tok a])
shuntingYardComplete xs = shuntingYardSafe xs

syComplete :: String -> ShuntingYardResult ([Double], [Tok Double]) 
syComplete = parseAndEvalSafe 
  (parseComplete operatorTable functionTable) 
  (\ts -> shuntingYardComplete (BrckOpen : ts ++ [BrckClose]))  