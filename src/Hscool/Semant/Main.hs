module Main where
import           Control.Applicative     ((<$>))
import qualified Data.ByteString         as B
import           System.IO               (hPutStrLn, stderr)
import           Hscool.Semant.GlobalEnv (assureMain, getGlobalEnv)
import           Hscool.Semant.TypeCheck
import           Hscool.Semant.TypeEnv   (getTypeEnv)
import           Hscool.Types.AST


main :: IO ()
main = do
    program@(Program classes) <- parseUProgram <$> B.getContents
    let result = do
        typeEnv <- getTypeEnv classes
        globalEnv <- getGlobalEnv typeEnv
        typed_program <- typeCheck globalEnv program
        assureMain globalEnv
        return typed_program

    case result of
        Left msg -> hPutStrLn  stderr $ "Error: " ++ msg
        Right r -> putStr $ show r
