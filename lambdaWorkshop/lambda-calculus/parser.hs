module Parser (parseFile) where

import Data.Void
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L 
import Ast.Main
import Ast.Core.Main

type Parser = Parsec Void String

parseFile   =  parse  
                    ((fmap Lib parseModule  ) <|> 
                    (fmap Exec parseProgram))
                    ""
                                                               
parseModule :: Parser Module                                   
parseModule = do                                               
    symbol "Module"       ;                                    
    n <- identifier       ;
    symbol "where"        ;
    parseDefinitions      ;

parseDefinitions :: Parser Module
parseDefinitions = 
    try (eof >> return Empty ) 
    <|>
    ( do
    symbol "Import"               ;
    n    <- identifier            ;
    rest <- parseDefinitions      ;
    return $ Using n rest         ;
    ) <|> ( do 
    n      <- identifier          ;
    symbol ":-"                   ;
    e      <- parseExpr           ;
    symbol "-:"                   ;
    rest   <- parseDefinitions    ;
    return $  Definition n e rest ;
    )

parseProgram :: Parser Program
parseProgram = 
    try ( do 
    symbol "main"                 ;
    symbol ":-"                   ;
    e <- parseExpr                ;
    symbol "-:"                   ;
    eof                           ;
    return $ Main e               ;
    ) <|> ( do
    symbol "Import"               ;
    n <- identifier               ;
    rest <- parseProgram          ;
    return $ Import n rest        ;
    ) <|> ( do 
    n      <- identifier          ;
    symbol ":-"                   ;
    e      <- parseExpr           ;
    symbol "-:"                   ;
    rest   <- parseProgram        ;
    return $  Prelude n e rest    ;
    )


assocExp :: [ Expr ] -> Expr
assocExp []       = error "empty assocExp"
assocExp (x:[])   = x
assocExp (x:y:xs) = assocExp ((APP x y):xs)

parseExpr :: Parser Expr
parseExpr =  parseExpr' >>= (return . assocExp)

parseExpr' :: Parser [Expr]
parseExpr' = (try $
                do
                e    <- simpleExpr
                rest <- parseExpr'
                return $ e:rest)
                <|> return []

simpleExpr :: Parser Expr
simpleExpr = 
    try force' <|>
    try tick   <|>
    try lambda <|>
    try local  <|>
    ident

force' :: Parser Expr
force' = symbol "!" >> local >>= return . FORCE

lambda :: Parser Expr
lambda =  do 
        symbol "lambda"           ;
        ab <- identifier          ;
        symbol "."                ;
        e <- parseExpr            ;
        return $ LAMBDA (ID ab) e ;

tick :: Parser Expr
tick = symbol "'" >> local >>= return . TICK
ident :: Parser Expr
ident = 
        try ( do 
            symbol "@"           ;
            n <- identifier      ;
            return (Call n)      ;
        ) <|> ( do 
            n <- identifier      ;
            if n == "lambda" then fail ""
            else return (ID n)   ;)

local :: Parser Expr
local = 
        do
        symbol "("      ;
        e <- parseExpr  ;
        symbol ")"      ;
        return e        ;

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = (lexeme . try) p 
  where
    p       = (:) <$> letterChar <*> many alphaNumChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "(*" "*)"
