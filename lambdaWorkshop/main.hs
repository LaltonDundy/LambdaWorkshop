import System.IO
import Control.Exception 
import Control.Monad
import Text.Megaparsec.Error
import Data.List.NonEmpty ( NonEmpty(..) )

import LambdaCalculus.Main

main :: IO ()
main = introduce >> main'

main' :: IO ()
main' = do
    fileName <- askForFile >> getLine
    putStrLn "" 
    join $
        case categorize fileName of
            LambdaCalc -> handleOutcome <$> runThenOutCome LambdaCalc fileName
            NonSupport -> userFail >> return main'
            Quit       -> (return . putStrLn $ "bye bye") >> return . return $ ()

introduce :: IO ()
introduce = 
    putStrLn "Hello, welcome to LambdaWorkshop"

askForFile :: IO ()
askForFile =
    putStrLn  "Please, enter file that you want to explore: "

userFail :: IO ()
userFail = 
    putStrLn "Sorry, this doesn't appear to be an option"

data FileType = LambdaCalc | NonSupport | Quit

categorize :: String -> FileType
categorize str = 
    case reverse str of
        ('c':'l':'.':_) -> LambdaCalc
        ('q':[])        -> Quit
        _               -> NonSupport

data Outcome = Success | FileNonExist | ParseFailure

sloadFile :: FilePath -> IO (Maybe String)
sloadFile p = catch (fmap Just $ readFile p) handler
   where
   handler :: IOException -> IO (Maybe String)
   handler _ = return Nothing

runThenOutCome :: FileType -> String -> IO Outcome
runThenOutCome LambdaCalc str = do
    output <- sloadFile str
    case output of
        Nothing   -> return FileNonExist
        Just file ->
            case parseFile file of
            Left  err   -> handleParseError err >> return ParseFailure
            Right ast -> do 
                    compiled <- compile ast
                    case compiled  of
                        ( _ , Just prog ) -> do 
                                                prettyPrint prog
                                                result <- eval prog 
                                                success 
                                                prettyPrint result
                                                return Success
                        _                 -> error "Maintainer error: runThenOutCome LambdaCalc Module structure needed"

handleParseError (ParseErrorBundle (first :| otherErrs) pos) =
    (sequence $ fmap (putStrLn . parseErrorPretty) (first : otherErrs))

handleOutcome :: Outcome -> IO ()
handleOutcome o =
    case o of
    Success      -> idle
    FileNonExist -> putStrLn "File does not exist"         >> main'
    ParseFailure -> putStrLn "Could not parse given file"  >> main'

idle = main'
success = putStrLn "==================================================" 
