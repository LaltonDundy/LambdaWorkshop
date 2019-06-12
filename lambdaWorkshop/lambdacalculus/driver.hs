import System.Environment

import LambdaCalculus.Main

main =  do
    lst              <- getArgs
    f                <- readFile . head $ lst
    Right file       <- return $ parseFile f 
    ( _ , compiled ) <- compile file
    case compiled of 
        Just v  -> prettyPrint v         >> 
                   print "=============" >>
                   eval v >>= prettyPrint
        Nothing -> error "non-executable"
