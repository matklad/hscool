module Main where
import qualified Data.ByteString       as B
import           Data.List             (nub, (\\))
import           Hscool.Semant.TypeEnv
import           Hscool.Semant.TypeCheck
import           Hscool.Types.AST


main :: IO ()
main = do
  input <- B.getContents
  let program =  parseUProgram input
  let types = getTypes program
  let result = do
      types' <- simpleCheck types
      typeEnv <- getTypeEnv types'
      typeCheck typeEnv program
      
  case result of
    Left msg -> print $ "Error: " ++ msg
    Right r -> print r

getTypes :: UProgram -> [(String, String)]
getTypes (Program cs) = map (\(Class name super _ _) -> (name, super)) cs

simpleCheck :: [(String, String)] -> Either String [(String, String)]
simpleCheck types =
  case classes \\ nub classes of
    [] -> Right $ [("Object", "_"),
                   ("IO", "Object"),
                   ("Int", "Object"),
                   ("String", "Object"),
                   ("Bool", "Object")] ++ types
    cs -> Left $ "Class redeclaration: " ++ show cs
  where
    classes = buildin ++ [x | (x, _) <- types]
    buildin = ["Object", "IO", "Int", "String", "Bool", "SELF_TYPE"]
