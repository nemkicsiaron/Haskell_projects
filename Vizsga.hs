{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Bool

-- State monad
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------
--                              Feladatok
--------------------------------------------------------------------------------

data UList a
  = Empty
  | Cons a (UList a) a
    deriving (Show)

instance Eq a => Eq (UList a) where
  (==) Empty           Empty           = True
  (==) (Cons la ll lb) (Cons ra rl rb) = la == ra && lb == rb && ll == rl
  (==) _                _              = False

instance Functor UList where
  fmap f Empty        = Empty
  fmap f (Cons a l b) = Cons (f a) (fmap f l) (f b)

instance Foldable UList where
  foldr _ acc Empty        = acc
  foldr f acc (Cons a l b) = f a (foldr f (f b acc) l)

instance Traversable UList where
  traverse f Empty = pure Empty
  traverse f (Cons a l b) = Cons <$> f a <*> traverse f l <*> f b

reverseUList :: UList a -> UList a
reverseUList Empty = Empty
reverseUList (Cons a l b) = Cons b (reverseUList l) a

maxByKey :: Ord k => (a -> k) -> a -> a -> a
maxByKey f x y = if (f x) >= (f y) then x else y

maximumByKey :: Ord k => (a -> k) -> UList a -> Maybe a
maximumByKey _ Empty = Nothing
--maximumByKey f (Cons a (Empty) b) = Just (max (f a) (f b))
maximumByKey f ul = Just (foldr1 (maxByKey f) ul)

data Tree a b
  = Leaf  a b
  | Node (Tree a b) (Tree a b)
  deriving (Eq, Show)

labelBs :: Tree a b -> Tree a (b, Int)
labelBs t = evalState (go t) 0 
  where
    go :: Tree a b -> State Int (Tree a (b, Int))
    go (l `Node` r) = do
      l <- go l
      r <- go r
      pure $ l `Node` r
    go (a `Leaf` b) = do
      n <- get 
      put $ n + 1
      pure $ Leaf a (b, n)

labelByLevel :: Tree a b -> Tree a (b, Int)
labelByLevel t = evalState (go t) 0 
  where
    go :: Tree a b -> State Int (Tree a (b, Int))
    go (l `Node` r) = do
      n <- get 
      put $ n + 1
      l <- go l
      r <- go r
      pure $ l `Node` r
    go (a `Leaf` b) = do
      n <- get 
      pure $ Leaf a (b, n)

deepestB :: Tree a b -> b
deepestB t = fst $ evalState (go $ labelByLevel t) (undefined, 0) 
  where
    go :: Tree a (b, Int) -> State (b, Int) (b, Int)
    go (l `Node` r) = do 
      r <- go r
      l <- go l
      get
    go (a `Leaf` (b, c)) = do
      (maxB, n) <- get
      put $ bool (maxB, n) (b, c) (c >= n)
      get


--------------------------------------------------------------------------------
--                  While nyelv parser + interpreter
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

anyChar :: Parser Char
anyChar = satisfy (const True)

-- While nyelv
------------------------------------------------------------

data Exp
  = Add Exp Exp    -- a + b
  | Mul Exp Exp    -- a * b
  | Var Name       -- x
  | IntLit Int
  | BoolLit Bool   -- true|false
  | Not Exp        -- not e
  | And Exp Exp    -- a && b
  | Or Exp Exp     -- a || b
  | Eq Exp Exp     -- a == b
  | Lt Exp Exp     -- a < b
  | ListLit [Exp]
  | Append Exp Exp
  | Length Exp
  | Head Exp
  | Tail Exp
  deriving (Eq, Show)

--ListLit :: [Exp] -> Exp, Append :: Exp -> Exp -> Exp, Length :: Exp -> Exp, Head :: Exp -> Exp és Tail :: Exp -> Exp

type Program = [Statement]
type Name    = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}       (lokális scope)
  deriving (Eq, Show)


-- While parser
--------------------------------------------------------------------------------

{-
Parser a While nyelvhez. A szintaxist az Exp és Statement definíciónál látahtó
fenti kommentek összegzik, továbbá:

  - mindenhol lehet whitespace tokenek között
  - a Statement-eket egy Program-ban válassza el ';'
  - Az operátorok erőssége és assszociativitása a következő:
      infixr 2 ||
      infixr 3 &&
      infix  4 ==
      infix  4 <
      infixl 6 +
      infixl 7 *
      infixr 8 ++
  - "not" erősebben köt minden operátornál.
  - A kulcsszavak: not, and, while, do, if, end, true, false.
  - A változónevek legyenek betűk olyan nemüres sorozatai, amelyek *nem* kulcsszavak.
    Pl. "while" nem azonosító, viszont "whilefoo" már az!

Példa szintaktikilag helyes programra:

  x := 10;
  y := x * x + 10;
  while (x == 0) do
    x := x + 1;
    b := true && false || not true
  end;
  z := x
-}

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

keywords :: [String]
keywords = ["++", "[", "]", "length", "tail", "head", "not", "and", "while", "do", "if", "end", "true", "false"]

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pBoolLit :: Parser Bool
pBoolLit = (True  <$ string' "true")
       <|> (False <$ string' "false")

pIntLit :: Parser Int
pIntLit = read <$> (some (satisfy isDigit) <* ws)

pListLit :: Parser [Exp]
pListLit = char' '[' *> sepBy pExp (char' ',') <* char' ']'

pAtom :: Parser Exp
pAtom = (ListLit <$> pListLit)
      <|> (BoolLit <$> pBoolLit)
      <|> (IntLit <$> pIntLit)
      <|> (Var <$> pIdent)
      <|> (char' '(' *> pExp <* char' ')')

pHead :: Parser Exp
pHead = (Head <$> (string' "head" *> pAtom))
  <|> pAtom

pTail :: Parser Exp
pTail = (Tail<$> (string' "tail" *> pHead))
  <|> pHead

pLen :: Parser Exp
pLen = (Length <$> (string' "length" *> pTail))
  <|> pTail

pNot :: Parser Exp
pNot =
      (Not <$> (string' "not" *> pLen))
  <|> pLen

pApp :: Parser Exp
pApp = foldr1 Append <$> sepBy1 pNot (string' "++")

pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pApp (char' '*')

pAdd :: Parser Exp
pAdd = foldl1 Add <$> sepBy1 pMul (char' '+')

pEqOrLt :: Parser Exp
pEqOrLt =
  pAdd >>= \e ->
        (Eq e <$> (string' "==" *> pAdd))
    <|> (Lt e <$> (string' "<"  *> pAdd))
    <|> pure e

pAnd :: Parser Exp
pAnd = foldr1 And <$> sepBy1 pEqOrLt (string' "&&")

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "||")

pExp :: Parser Exp
pExp = pOr

pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')

pStatement :: Parser Statement
pStatement =
        (Assign <$> pIdent <*> (string' ":=" *> pExp))
    <|> (While <$> (string' "while" *> pExp)
               <*> (string' "do" *> pProgram <* string' "end"))
    <|> (If <$> (string' "if"   *> pExp)
            <*> (string' "then" *> pProgram)
            <*> (string' "else" *> pProgram <* string' "end"))
    <|> (Block <$> (char' '{' *> pProgram <* char' '}'))

pSrc :: Parser Program
pSrc = ws *> pProgram <* eof

-- Interpreter
------------------------------------------------------------

{-
Interpreter a While nyelvhez.

Kifejezések:
  - A logikai és artimetikai műveletek kiértékelése értelemszerű. Ha nem
    megfelelő típusú értéket kapunk argumentumokra, dobjunk "error"-al hibát.
  - Az == operátor működik, ha mindkét argumentum Bool, vagy ha mindkét argumentum
    Int, az eredmény mindig Bool.

Változó scope és értékadás kezelése:
  - Új scope-nak számít:
    - minden "while" kifejezés teste
    - minden "if" kifejezés két ága
    - minden új Block (a szintaxisban pl "x := 0; {y := x; x := x}"-nél
      a kapcsos zárójeles utasítássorozat új blokkban van).

  - ha egy új változónak értéket adunk, akkor felvesszük a környezet elejére
  - ha egy meglévő változónak értéket adunk, akkor update-eljük a változó értékét
  - amikor az interpreter végez egy scope kiértékeléséval, eldobja az összes,
    scope-ban újonnan felvett változót a környezetből.
-}

data Val = VInt Int | VBool Bool | VList [Val] deriving (Eq, Show)
type Env  = [(Name, Val)]
type Eval = State Env

binOp :: String
      -> Exp
      -> Exp
      -> Either (Int -> Int -> Int) (Bool -> Bool -> Bool)
      -> Eval Val
binOp opName e1 e2 f = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (f, v1, v2) of
    (Left f , VInt n1 , VInt n2 ) -> pure (VInt (f n1 n2))
    (Right f, VBool b1, VBool b2) -> pure (VBool (f b1 b2))
    _                             -> error ("type error in " ++ opName ++ " argument")

evalExp :: Exp -> Eval Val
evalExp e = case e of
  Add e1 e2 -> binOp "+"  e1 e2 (Left (+))
  Mul e1 e2 -> binOp "*"  e1 e2 (Left (*))
  And e1 e2 -> binOp "&&" e1 e2 (Right (&&))
  Or  e1 e2 -> binOp "||" e1 e2 (Right (||))
  Eq  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (VInt  n1, VInt  n2) -> pure (VBool (n1 == n2))
      (VBool b1, VBool b2) -> pure (VBool (b1 == b2))
      _                    -> error "type error in == arguments"
  Lt  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (VInt  n1, VInt  n2) -> pure (VBool (n1 < n2))
      _                    -> error "type error in < arguments"
  Var x    -> do
    env <- get
    case lookup x env of
      Nothing -> error ("variable not in scope: " ++ x)
      Just v  -> pure v
  IntLit  n -> pure (VInt  n)
  BoolLit b -> pure (VBool b)
  Not e -> do
    v <- evalExp e
    case v of
      VBool b -> pure (VBool (not b))
      _       -> error "type error in \"not\" argument"
  ListLit l -> do 
    ls <- mapM evalExp l
    pure (VList ls)
  Append ls e -> do
    e <- evalExp e
    ls <- evalExp ls
    case ls of
      VList l -> case e of
                  VList e -> pure $ VList $ l ++ e
                  _       -> pure $ VList $ l ++ [e]
      _       -> error "type error in \"++\" argument"
  Length ls -> do
    ls <- evalExp ls
    case ls of 
      VList ls -> pure (VInt $ length ls)
      _       -> error "type error in \"length\" argument"
  Head ls -> do
    ls <- evalExp ls
    case ls of
      VList [] -> error "type error in \"head\" argument"
      VList (l:ls) -> pure l
      _       -> error "type error in \"head\" argument"
  Tail ls -> do
    ls <- evalExp ls
    case ls of
      VList [] -> error "type error in \"tail\" argument"
      VList (l:ls) -> pure $ VList ls
      _       -> error "type error in \"tail\" argument"

newScope :: Eval a -> Eval a
newScope ma = do
  env <- (get :: State Env Env)
  a <- ma
  modify (\env' -> drop (length env' - length env) env')
  pure a

updateEnv :: Name -> Val -> Env -> Env
updateEnv x v env =
  case go env of
    Nothing   -> (x, v):env
    Just env' -> env'
  where
    go :: Env -> Maybe Env
    go [] = Nothing
    go ((x', v'):env)
      | x == x'   = Just ((x, v):env)
      | otherwise = ((x', v'):) <$> go env

evalSt :: Statement -> Eval ()
evalSt s = case s of
  Assign x e -> do
    v <- evalExp e
    modify (updateEnv x v)
  While e p -> do
    v <- evalExp e
    case v of
      VBool b -> if b then newScope (evalProg p) >> evalSt (While e p)
                      else pure ()
      _       -> error "type error: expected a Bool condition in \"while\" expression"
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      VBool b -> if b then newScope (evalProg p1)
                      else newScope (evalProg p2)
      _       -> error "type error: expected a Bool condition in \"if\" expression"
  Block p ->
    newScope (evalProg p)

evalProg :: Program -> Eval ()
evalProg = mapM_ evalSt


-- interpreter
--------------------------------------------------------------------------------

runProg :: String -> Env
runProg str = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (p, _) -> execState (evalProg p) []

p1 :: String
p1 = "x := 10; y := 20; {z := x + y}; x := x + 100"