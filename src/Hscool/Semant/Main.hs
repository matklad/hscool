module Main where
import           Control.Applicative     ((<$>))
import qualified Data.ByteString         as B
import           Hscool.Semant.TypeCheck
import           Hscool.Semant.TypeEnv(getTypeEnv)
import           Hscool.Semant.GlobalEnv(getGlobalEnv)
import           Hscool.Types.AST


main :: IO ()
main = do
  program@(Program classes) <- parseUProgram <$> B.getContents
  let result = do
          typeEnv <- getTypeEnv classes
          globalEnv <- getGlobalEnv typeEnv classes
          typeCheck globalEnv program


  case result of
    Left msg -> print $ "Error: " ++ msg
    Right r -> putStr $ show r
