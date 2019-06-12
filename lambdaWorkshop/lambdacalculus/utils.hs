module LambdaCalculus.Utils (
parseFile   ,
eval        ,
compile     ,
prettyPrint 
)
where

import Parser        (parseFile)
import Ast.Core.Main (eval)
import Ast.Main      (compile, prettyPrint)
