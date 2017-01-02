{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Applicative
import Data.Either
import Data.List
import Data.Char

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

parseConst :: a -> Parser a
parseConst a = Parser $ \s -> [(a, s)]

parseString :: String -> Parser String
parseString target = Parser p where
  p str | target `isPrefixOf` str = [(target, drop (length target) str)]
        | otherwise               = []


instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [(f a, s') | (a, s') <- p s]

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  Parser pf <*> Parser pa = Parser p where
    p s = [(f a, s'') | (f, s') <- pf s, (a, s'') <- pa s']

instance Alternative Parser where
  empty = Parser $ const []
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s ++ p2 s

runParserSimple :: Parser a -> String ->  Maybe a
runParserSimple (Parser p) s = let complete = [a | (a, s') <- p s, null s']
                               in  if null complete
                                   then Nothing
                                   else Just (head complete)

star :: Parser a -> Parser [a]
star p = ((:) <$> p <*> star p) <|> (parseConst [])

plus :: Parser a -> Parser [a]
plus p = (:) <$> p <*> star p

parseAny :: [Parser a] -> Parser a
parseAny = foldr (<|>) empty

space :: Parser ()
space = Parser $ \s -> [((), dropWhile isSpace s)]

spaced :: Parser a -> Parser a
spaced p = space *> p <* space

parened :: Parser a -> Parser a
parened p = parseString "(" *> p <* parseString ")"

parenedCurly :: Parser a -> Parser a
parenedCurly p = parseString "{" *> p <* parseString "}"

sep1 :: Parser a -> String -> Parser [a]
sep1 p s = (:) <$> p <*> star (parseString s *> p)


---------------------------------------


type Name = String
type TypeArg = String
data Evaluation = Deferred | Forced deriving (Eq, Show)

data Module = Module [DataDecl] [Definition] deriving Show

data DataDecl = DataDecl Name [TypeArg] [Constructor] deriving Show

data Constructor = Constructor Name [Type] deriving Show

data Type = Type Name | TypeApp Type Type deriving Show

data Definition = Definition Name (Expr 'Deferred) deriving Show

data Expr :: Evaluation -> * where
  ERef :: Name -> Expr 'Deferred
  EApp :: Expr 'Forced -> Expr 'Deferred -> Expr 'Forced
  ELambda :: [Name] -> Expr 'Forced -> Expr 'Forced
  ECase :: Expr 'Deferred -> [(Pattern, Expr 'Deferred)] -> Expr 'Deferred
  EString :: String -> Expr 'Forced
  EForce :: Expr 'Deferred -> Expr 'Forced
  EDefer :: Expr 'Forced -> Expr 'Deferred
deriving instance Show (Expr a)

data Pattern = Pattern Name [Name] deriving Show

---------------------------------------

preProcess :: String -> String
preProcess input = go input where
  go []           = []
  go ('{':'-':xs) = preProcess $ dropComment xs
  go (x:xs)       = x : preProcess xs

  dropComment []           = []
  dropComment ('-':'}':xs) = xs
  dropComment (_:xs)       = dropComment xs



----------------------------------------

parseModule :: Parser Module
parseModule =
  fmap toModule $ star $ parseAny [Left <$> parseDecl, Right <$> parseDef]
  where
    toModule entries = let decls = lefts entries
                           defs = rights entries
                      in  Module decls defs

parseDecl :: Parser DataDecl
parseDecl =
  let name = spaced (parseString "data") *> parseIdentifier <* spaced (parseString "=")
      constructors = sep1 parseConstructor "|" <* spaced (parseString ";")
  in  DataDecl <$> name <*> pure [] <*> constructors

parseIdentifier :: Parser String
parseIdentifier = Parser $ \s ->
  let identifier = takeWhile isAlphaNum s
  in  if null identifier
      then []
      else [(identifier, drop (length identifier) s)]

parseConstructor :: Parser Constructor
parseConstructor =
  let name = spaced parseIdentifier
      args = star parseTypeNoApp
  in  Constructor <$> name <*> args

parseTypeNoApp :: Parser Type
parseTypeNoApp = parseSimple <|> parseParened where
  parseSimple = spaced $ Type <$> parseIdentifier
  parseParened = spaced $ parened $ parseType

parseType :: Parser Type
parseType = parseAll where
  parseMany = sep1 parseTypeNoApp ""
  parseAll = foldl1 TypeApp <$> parseMany

parseDef :: Parser Definition
parseDef = Definition
  <$> (spaced parseIdentifier <* spaced (parseString "="))
  <*> (spaced parseExpr <* parseSemi)

parseNonAppExpr :: Parser (Expr 'Deferred)
parseNonAppExpr = parseAny [wrapped, estring, elambda, ecase, eref]  where
  eref = ERef <$> spaced parseIdentifier

  elambda = let lam = spaced (parseString "\\")
                names = sep1 (spaced parseIdentifier) ""
                f args body = EDefer (ELambda args (EForce body))
            in  f <$> (lam *> names <* parseArrow) <*> parseExpr

  ecase =
    let case_ = spaced $ parseString "case"
        of_ = spaced $ parseString "of"
        lhs = case_ *> parseExpr <* of_
        branch = (,)
          <$> (parsePattern <* parseArrow)
          <*> (parseExpr <* parseSemi)
        rhs = parenedCurly $ star branch
    in  ECase <$> lhs <*> rhs

  -- TODO \"
  estring = let rest = Parser $ \s -> let v = takeWhile (/= '"') s
                                      in  [(v, drop (length v + 1) s)]
            in  spaced $ EDefer . EString <$> (parseString "\"" *> rest)

  wrapped = spaced $ parened parseExpr

parseExpr :: Parser (Expr 'Deferred)
parseExpr = foldl1 f <$> sep1 (spaced parseNonAppExpr) "" where
  f e1 e2  = EDefer $ EApp (EForce e1) e2


parsePattern :: Parser Pattern
parsePattern =
  let name = spaced parseIdentifier
      args = star name
  in  Pattern <$> name <*> args

parseArrow :: Parser ()
parseArrow = spaced (parseString "->") *> pure ()

parseSemi :: Parser ()
parseSemi = spaced (parseString ";") *> pure ()

---------------------------------------------------

jsModule :: Module -> String
jsModule (Module decls defs) =
  unlines $ jsPrelude : map jsDecl decls ++ map jsDef defs ++ ["main();"]

jsPrelude :: String
jsPrelude = unlines
  [ "const __thunk=function(a){"
  , "  var b,c;return b=!1,c=null,function(){return b||(c=a(),b=!0),c}"
  , "};"
  , "const print = __thunk(() => (x) => console.log(x()));"
  , "const concat = __thunk(() => (a) => (b) => a() + b());"
  ]

jsDecl :: DataDecl -> String
jsDecl (DataDecl _ _ constructors) =
  unlines $ map jsConstructor constructors

jsConstructor :: Constructor -> String
jsConstructor (Constructor name types) =
  let args = map (\n -> "_" ++ show n) $ take (length types) [1 :: Int ..]
      curriedArgs = map (++ " =>") args
      lhs = if null curriedArgs then "" else unwords curriedArgs
      arr = "  const arr = [" ++ intercalate ", " args ++ "];"
      tag = "  arr.__constructor = " ++ show name ++ ";"
      ret = "  return arr;"
      rhs = unlines ["(function(){", arr, tag, ret, "}())"]
  in  "const " ++ name ++ " = __thunk(() => " ++ lhs ++ rhs ++ ");"

jsDef :: Definition -> String
jsDef (Definition name expr) =
  "const " ++ name ++ " = " ++ jsExpr expr ++ ";"

jsExpr :: Expr a -> String
jsExpr expr = case expr of
  ERef name ->
    name

  EApp e1 e2 ->
    "(" ++ jsExpr e1 ++ ")(" ++ jsExpr e2 ++ ")"

  ELambda args body ->
    let curriedArgs = map (++ " =>") args
        jsArgs = if null curriedArgs then "" else unwords curriedArgs
    in  jsArgs ++ " " ++ jsExpr body

  ECase scrutinee patterns ->
    let value = "const __value = " ++ jsExpr (EForce scrutinee) ++ ";"
        missing = [ "default: "
                  , "throw 'Missing pattern: ' + __value.__constructor + ' for ' + __value"
                  ]
        cases = unlines $ map mkCase patterns ++ missing
        mkCase (Pattern cons args, rhs) =
          let assignments = "const [" ++ intercalate ", " args ++ "] = __value";
          in  "case " ++ show cons ++ ": \n" ++
                assignments ++ "\nreturn " ++ jsExpr rhs

    in  "(function(){\n" ++ value ++ "\nswitch (__value.__constructor) {\n" ++ cases ++ "}}())"

  EString s ->
    show s

  EForce e ->
    "(" ++ jsExpr e ++ ")()"

  EDefer e ->
    "__thunk(() => " ++ jsExpr e ++ ")"

---------------------------------------------------

main :: IO ()
main = do
  raw <- getContents
  let input = preProcess raw
      ast = runParserSimple parseModule input
      debugParser = unlines $ map show $ runParser parseModule input
      js  = jsModule <$> ast
  maybe (putStrLn $ "failed to parse: " ++ debugParser)
         putStrLn js
