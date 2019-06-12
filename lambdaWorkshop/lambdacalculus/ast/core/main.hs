module LambdaCalculus.Ast.Core.Main 
( Expr(..)
, betaReduce
, getFrees
, getMax
, idsToLabels
, increaseBY
, eval )
where

import LambdaCalculus.Ast.Core.Internals
