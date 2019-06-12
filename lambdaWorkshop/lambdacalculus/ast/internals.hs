module LambdaCalculus.Ast.Internals where

import Data.Maybe

import LambdaCalculus.Ast.Core.Main

data Program = 
            Main Expr
                | Import { fname :: String , rest_ :: Program }
                | Prelude 
                { label :: String
                , body  :: Expr
                , tl    :: Program }
               deriving (Show)

data Module = 
    Empty 
        | Using { fnm :: String , other :: Module }
        | Definition 
        { name :: String
        , ex   :: Expr
        , rest :: Module }
        deriving (Show , Read )

data File = Lib Module | Exec Program
                deriving (Show)

type Env  = [ ( String , Expr ) ] 
compile :: File -> IO ( Env , Maybe Expr )
compile file = 
        let helper env file =
                case file of
                    -- Executable
                   Exec (Main e         )       -> do
                                e'         <-   return    . 
                                                alphaConv .
                                                snd       .
                                                head      .
                                                reverse   . 
                                                makeBasic . 
                                                map (\(x,y) -> (x , idsToLabels y))  $ 
                                                env <> [ ("main" , e ) ]
                                return ( env , Just e' )
                   Exec (Import n rst)           -> 
                            do 
                                externs       <- readFile $ n <> ".li"
                                (newenv , _ ) <- (compile . Lib $ (read externs :: Module)) 
                                helper (newenv <> env) (Exec rst)
                   Exec (Prelude lbl bdy t)     -> 
                        helper 
                          (( lbl , idsToLabels bdy) : env) (Exec t)
                    -- Module
                   Lib  Empty                   -> return ( env , Nothing )
                   Lib (Using n rst)           -> 
                            do 
                                externs       <- readFile $ n <> ".li"
                                (newenv , _ ) <- (compile . Lib $ (read externs :: Module)) 
                                helper (newenv <> env) (Lib rst)
                   Lib  (Definition lbl bdy t)  -> 
                        helper 
                          ( (lbl , idsToLabels bdy) : env) (Lib t)
    in
    print "compiling" >> helper [] file

alphaConv :: Expr -> Expr
alphaConv expr = 
    case expr of 
        APP e1 e2 -> APP (alphaConv (increaseBY (getMax e2) e1)) e2
        rest -> rest

makeBasic :: Env -> Env
makeBasic env = 
    let replace str e1' e2' =
                case e2' of
                    Call s       -> if s == str then e1' else Call s
                    APP e1 e2    -> APP    (replace str e1' e1) (replace str e1' e2)
                    LAMBDA e1 e2 -> LAMBDA (replace str e1' e1) (replace str e1' e2)
                    TICK  e       -> TICK  $ replace str e1' e
                    FORCE e       -> FORCE $ replace str e1' e
                    rest         -> rest
    in
   case env of 
       []          -> []
       ((x,y):xs) -> 
            let newLst = map (\ (s , a) -> (s ,  replace x (increaseBY (getMax a) y) $ a ) ) xs in
            (x,y) : makeBasic newLst
