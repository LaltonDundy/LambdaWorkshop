module Ast.Serialize where

import Ast.Core.Internals
import Text.Show.Unicode
import System.Console.Pretty

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
        TICK e   -> "'" <> (serialize e)
        FORCE e  -> '!' : (serialize e)

prettyPrint :: Expr -> IO ()
prettyPrint = putStrLn . serialize
