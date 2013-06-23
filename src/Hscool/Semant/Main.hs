module Main where
import qualified Data.ByteString       as B
import           Hscool.Semant.TypeEnv
import           Hscool.Semant.TypeCheck
import           Hscool.Types.AST


main :: IO ()
main = do
  input <- B.getContents
  let program@(Program classes) =  parseUProgram input
  let result = do
      typeEnv <- getTypeEnv classes
      typeCheck typeEnv program
      
  case result of
    Left msg -> print $ "Error: " ++ msg
    Right r -> print r
