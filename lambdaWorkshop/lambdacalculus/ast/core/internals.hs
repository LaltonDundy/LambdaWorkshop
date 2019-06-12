module LambdaCalculus.Ast.Core.Internals where

data Expr =
      ID String
    | LABEL Int
    | Call String
    | APP Expr Expr
    | LAMBDA Expr Expr
    | TICK Expr
    | FORCE Expr
    deriving ( Read, Show , Eq )

betaReduce n body sub = 
    case body of 
        LABEL n'         ->  if n' == n then sub else LABEL n'
        APP f arg        -> 
            APP 
            (betaReduce n f sub) 
            (betaReduce n arg sub)
        LAMBDA name body -> LAMBDA name (betaReduce n body sub)
        Call   str       -> Call str
        TICK   e         -> TICK  $ betaReduce n e sub
        FORCE  e         -> FORCE $ betaReduce n e sub
        _                -> error "Compiler error: IDs are not meant to be left in at this point"

getFrees :: Expr -> [ String ]
getFrees expr = 
    helper expr []
        where helper e lst = 
                case e of 
                    ID str           -> if str `elem` lst then lst else str : lst
                    APP f arg        -> helper f (helper arg lst)
                    LAMBDA name body -> helper name (helper body lst) 
                    TICK  e'         -> helper e' lst
                    FORCE e'         -> helper e' lst
                    rest             -> lst

getMax :: Expr -> Int
getMax =
    helper 1 
        where helper n e =
                case e of 
                    LABEL n'         -> max n' n
                    APP f arg        -> max (helper n f) (helper n arg)
                    LAMBDA name body -> max (helper n name) (helper n body)
                    TICK  e'         -> helper n e'
                    FORCE e'         -> helper n e'
                    _                -> n

idsToLabels :: Expr -> Expr
idsToLabels expr =
    let frees = getFrees expr               in
    let table = zip frees [1..length frees] in
        helper table expr 
            where helper t e = 
                    case e of 
                        ID str           -> case lookup str t of 
                                                Just v  -> LABEL v
                                                Nothing -> error "Compiler error: getFrees should return all items"
                        APP f arg        -> APP (helper t f) (helper t arg)
                        LAMBDA name body -> LAMBDA (helper t name) (helper t body)
                        TICK  e'         -> TICK  $ helper t e'
                        FORCE e'         -> FORCE $ helper t e'
                        rest             -> rest

increaseBY :: Int -> Expr -> Expr
increaseBY n expr = 
    case expr of
        LABEL n'     -> LABEL $ n' + n
        APP f arg    -> APP (increaseBY n f) (increaseBY n arg)
        LAMBDA e1 e2 -> LAMBDA (increaseBY n e1) (increaseBY n e2)
        TICK  e      -> TICK  $ increaseBY n e
        FORCE e      -> FORCE $ increaseBY n e
        rest         -> rest

evaluatable :: Expr -> Bool
evaluatable (APP (LAMBDA _ _) _ ) = True
evaluatable (APP a b)             = evaluatable a || evaluatable b
evaluatable (FORCE e)             = evaluatable e
evaluatable _                     = False

eval :: Expr -> IO Expr
eval expr = 
    case expr of
        (APP (LAMBDA (LABEL n) body ) arg) -> eval arg  >>= \arg' -> eval   $ betaReduce n body arg' 
        (APP (LABEL n)  arg)               -> eval arg  >>= \arg' -> return $ APP (LABEL n) arg'
        (APP a b)                          -> do
                                                e2 <- eval b
                                                e1 <- eval a
                                                if evaluatable (APP e1 e2) 
                                                    then eval    $ APP e1 e2
                                                    else return  $ APP e1 e2
        LAMBDA e body                      -> eval body >>= return . LAMBDA e
        FORCE (TICK e)                     -> eval e
        FORCE e                            -> eval e
        rest                               -> return rest
