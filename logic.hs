{-# LANGUAGE GADTs #-}
import Parsing2
import qualified Data.Map as M
import           Text.Read          (readMaybe)
import           System.Environment (getArgs)

------------------------------------------
------------ Type Definitions ---------
------------------------------------

type Var = String
type Prog = [Stmt]
type Mem = M.Map Var Expr

------------------------------------------
------------ AST Data Constructors ----
------------------------------------

data Stmt where
    Assign :: Var  -> Expr -> Stmt -- <var> '=' <expr>
    Input  :: Var  -> Stmt         -- 'input' <var>
    Output :: Expr -> Stmt         -- 'output' <expr>
    deriving Show

data Expr where
    EBool :: Bool -> Expr                    -- 'False' | 'True'
    EVar  :: Var  -> Expr                    -- <var>
    EUn   :: UOp  -> Expr -> Expr            -- <uop> <expr>
    EBin  :: BOp  -> Expr -> Expr -> Expr    -- <expr> <bop> <expr>
    deriving Show

data UOp = Not
    deriving (Show, Eq)

data BOp = And | NAnd | Or | XOr | NOr
    deriving (Show, Eq)

------------------------------------------
------------ Parser -------------------
------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser $
    emptyDef
    { reservedNames   = [ "var", "input", "output", "True", "False", "xor"
                        , "nor", "nand", "and", "or", "not"]
    , reservedOpNames = [ "&", "|" , "~" ]
    }

parens :: Parser a -> Parser a
parens = getParens lexer

reserved, reservedOp :: String -> Parser ()
reserved   = getReserved lexer
reservedOp = getReservedOp lexer

ident :: Parser String
ident = getIdentifier lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

parseAtom :: Parser Expr
parseAtom
  =   EBool True  <$  reserved "True"
  <|> EBool False <$  reserved "False"
  <|> EVar        <$> ident
  <|> parens parseExpr

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseAtom
  where
    table = [ [ unary  "~"    (EUn Not)
              , unary "not"   (EUn Not) ]
            , [ binary "&"    (EBin And) AssocRight
              , binary "and"  (EBin And) AssocRight
              , binary "nand" (EBin NAnd) AssocRight]
            , [ binary "|"    (EBin Or)  AssocRight
              , binary "or"   (EBin Or)  AssocRight
              , binary "nor"  (EBin NOr) AssocRight
              , binary "xor"  (EBin XOr) AssocRight]
            ]
    unary  name fun       = Prefix (fun <$ reservedOp name)
    binary name fun assoc = Infix  (fun <$ reservedOp name) assoc

parseProg :: Parser Prog
parseProg = parseStmt `sepBy` (reservedOp ";")

parseStmt :: Parser Stmt
parseStmt =
        Input   <$> (reserved "input" *> ident)
    <|> Output  <$> (reserved "output" *> parseExpr)
    <|> Assign  <$> ident <*> (reservedOp "=" *> parseExpr)

logParser :: Parser Prog
logParser = whiteSpace *> parseProg <* eof

------------------------------------------
------------ Desugarer ----------------
------------------------------------

desugarProg :: Prog -> Prog
desugarProg [] = []
desugarProg (s:ss) = desugarStmt s : desugarProg ss

desugarExpr :: Expr -> Expr
desugarExpr (EBin XOr e1 e2)  =
    let a = desugarExpr e1
        b = desugarExpr e2
    in EBin And (EBin Or a b) (EUn Not (EBin And a b))
desugarExpr (EBin NOr e1 e2)  =
    let a = desugarExpr e1
        b = desugarExpr e2
    in EUn Not (EBin NOr a b)
desugarExpr (EBin NAnd e1 e2) =
    let a = desugarExpr e1
        b = desugarExpr e2
    in EUn Not (EBin And a b)
desugarExpr (EBin And e1 e2) =
    let a = desugarExpr e1
        b = desugarExpr e2
    in EBin And a b
desugarExpr (EBin Or e1 e2) =
    let a = desugarExpr e1
        b = desugarExpr e2
    in EBin Or a b
desugarExpr (EUn Not e1) =
    let a = desugarExpr e1
    in EUn Not a
desugarExpr (EBool b) = EBool b
desugarExpr (EVar v) = EVar v



desugarStmt :: Stmt -> Stmt
desugarStmt (Assign var e) = Assign var (desugarExpr e)
desugarStmt (Input var)    = Input var
desugarStmt (Output e)     = Output (desugarExpr e)
------------------------------------------
------------ Interpreter --------------
------------------------------------

interpExpr :: Mem -> Expr -> Bool
interpExpr _ (EBool b)      = b
interpExpr m (EVar x)       =
  case M.lookup x m of
    Just v  -> interpExpr m v
    Nothing -> error $ "Uninitialized variable " ++ x
interpExpr m (EBin b e1 e2) = interpBOp b (interpExpr m e1) (interpExpr m e2)
interpExpr m (EUn  u e)     = interpUOp u (interpExpr m e )

interpUOp :: UOp -> Bool -> Bool
interpUOp Not v = not v

interpBOp :: BOp -> Bool -> Bool -> Bool
interpBOp And    = \v1 v2 -> and [v1, v2]
interpBOp Or     = \v1 v2 -> or [v1, v2]

data World where
  W  :: Mem       -- Current state of memory
     -> [String]  -- Strings typed by the user, waiting to be read by 'input'
     -> [String]  -- Strings produced by 'output' (newest first)
     -> World
  Error :: World  -- Something went wrong
  deriving Show

-- An initial world state, given user input
initWorld :: String -> World
initWorld inp = W M.empty (words inp) []

interpStmt :: Stmt -> World -> World
interpStmt (Assign var e) (W m i o) = (W (M.insert var e m) i o)
interpStmt (Input var) w@(W m (i:is) o) = (W (M.insert var (EBool (toBool i)) m) is o)
interpStmt (Output e) w@(W m i o) = W m i (show (interpExpr m e) : o)
interpStmt _ _ = Error

toBool :: String -> Bool
toBool "True"  = True
toBool "T"     = True
toBool "1"     = True
toBool "False" = False
toBool "F"     = False
toBool "0"     = False

interpProg :: Prog -> World -> World
interpProg [] w = w
interpProg (s:ss) w = interpProg ss $ interpStmt s w

------------------------------------------
------------ File I/O -----------------
------------------------------------

formatWorld :: World -> Bool -> String
formatWorld (W m _ o) verbosity =
     unlines $
     ["-----"]
  ++ reverse o
  ++ ["-----"]
  ++ if verbosity then map formatVar (M.assocs m) else [""]
formatWorld Error _ = "Error"

formatVar (x,v) = x ++ " -> " ++ show v

run :: String -> Bool -> IO ()
run fileName verbosity = do
  s <- readFile fileName
  case parse logParser s of
    Left err -> print err
    Right p  ->
          do
          inp <- getContents
          let es = interpProg (desugarProg p) (initWorld inp)
          putStr $ formatWorld es verbosity

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Please provide a file name."
    (fn:("-v":_)) -> run fn True
    (fn:_) -> run fn False
