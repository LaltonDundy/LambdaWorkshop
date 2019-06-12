module LambdaCalculus.Ast.Serialize where

import Text.Show.Unicode
import System.Console.Pretty

import LambdaCalculus.Ast.Core.Internals

break_ = " "

serialize :: Expr -> String
serialize expr = 
    let lambda = (color Blue "\955") in
    case expr of
        ID str       -> str 
        LABEL n      -> 'L' : (show n)
        APP e1 e2    -> '(' : (serialize e1) 
                        <> " " <> (serialize e2) <> ")"
        LAMBDA e1 e2 -> "( " <> lambda <> " " <> (color Red . serialize $ e1)
                                     <> (color Green " . ") <> (serialize e2) <> " )"
        Call str -> '@':str
        TICK e   -> (color Green $  "'") <> (serialize e)
        FORCE e  -> (color Green $  "!")  <> (serialize e)

prettyPrint :: Expr -> IO ()
prettyPrint = putStrLn . serialize
