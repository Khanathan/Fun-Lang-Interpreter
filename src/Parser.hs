module Parser where

import AST
import Data.Char
import Control.Applicative

-- Parser monad
newtype Parser a = P (String -> [(a, String)])

-- Run a parser on input
parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

-- Basic parser instances
instance Functor Parser where
  fmap f p = P (\input -> case parse p input of
                            [] -> []
                            [(v, out)] -> [(f v, out)])

instance Applicative Parser where
  pure x = P (\input -> [(x, input)])
  pf <*> px = P (\input -> case parse pf input of
                             [] -> []
                             [(f, out)] -> parse (fmap f px) out)

instance Monad Parser where
  return = pure
  p >>= f = P (\input -> case parse p input of
                           [] -> []
                           [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\_ -> [])
  p1 <|> p2 = P (\input -> case parse p1 input of
                             [] -> parse p2 input
                             [(v, out)] -> [(v, out)])

-- Basic parsers
item :: Parser Char
item = P (\input -> case input of
                      [] -> []
                      (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat pred = do x <- item
              if (pred x) then return x else empty

char :: Char -> Parser Char
char c = sat (== c)

-- Whitespace handling
space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- String and identifier parsers
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- (sat isLower)
           xs <- many (sat isAlphaNum)
           return (x:xs)

-- Reserved keywords
keywords :: [String]
keywords = ["if", "then", "else", "let", "in", "true", "false"]

-- Parse an identifier that's not a keyword
identifier :: Parser String
identifier = do id <- token ident
                if id `elem` keywords
                  then empty
                  else return id

-- Integer literals
integer :: Parser Int
integer = do digits <- some (sat isDigit)
             return (read digits)

-- Parse a parenthesized expression
parens :: Parser a -> Parser a
parens p = do token (char '(')
              x <- p
              token (char ')')
              return x

-- Expression parser
expr :: Parser Expr
expr = letExpr <|> ifExpr <|> eqExpr

-- Expression application (function application or atomic)
appExpr :: Parser Expr
appExpr = do es <- some termExpr
             return (foldl1 App es)

-- Let expression parser
letExpr :: Parser Expr
letExpr = do token (string "let")
             var <- token identifier
             token (char '=')
             e1 <- expr
             token (string "in")
             e2 <- expr
             return (LetIn var e1 e2)

-- If-then-else expression parser
ifExpr :: Parser Expr
ifExpr = do token (string "if")
            e1 <- expr
            token (string "then")
            e2 <- expr
            token (string "else")
            e3 <- expr
            return (ITE e1 e2 e3)

-- Lambda abstraction parser
lambdaExpr :: Parser Expr
lambdaExpr = do token (char '\\') <|> token (char 'λ')
                var <- token identifier
                token (string "->")
                body <- expr
                return (Abs var body)

-- Equation parser (for comparisons)
eqExpr :: Parser Expr
eqExpr = do e1 <- addExpr
            token (string "==")
            e2 <- addExpr
            return (Equal e1 e2)
          <|> addExpr

-- Addition/subtraction parser
addExpr :: Parser Expr
addExpr = do e1 <- multExpr
             rest e1
          where
            rest e1 = (do token (char '+')
                          e2 <- multExpr
                          rest (Plus e1 e2))
                   <|> (do token (char '-')
                           e2 <- multExpr
                           rest (Minus e1 e2))
                   <|> return e1

-- Multiplication expression parser (placeholder for future)
multExpr :: Parser Expr
multExpr = appExpr

-- Term expression (atomic)
termExpr :: Parser Expr
termExpr = boolLiteral
       <|> intLiteral
       <|> variableExpr
       <|> lambdaExpr
       <|> parens expr

-- Boolean literal parser
boolLiteral :: Parser Expr
boolLiteral = do token (string "true")
                 return (CBool True)
              <|> do token (string "false")
                     return (CBool False)

-- Integer literal parser
intLiteral :: Parser Expr
intLiteral = do n <- token integer
                return (CInt n)

-- Variable reference parser
variableExpr :: Parser Expr
variableExpr = do var <- token identifier
                  return (Var var)

-- Parse a full expression
parseExpr :: String -> Either String Expr
parseExpr input =
  case parse expr input of
    []         -> Left "Parser error: no valid parse"
    [(e, "")]  -> Right e
    [(_, rest)] -> Left $ "Parser error: unconsumed input: " ++ rest
    _          -> Left "Parser error: ambiguous parse"