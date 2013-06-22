module Main where

import Hscool.Types.AST
import qualified Data.ByteString as B
import Data.List (nub, (\\))

main = do
  input <- B.getContents
  let program =  parseUProgram input
  let types = getTypes program
  case simpleCheck types of
    Left err -> print err
    Right () -> do
      let typeEnv = getTypeEnv types
      return ()

getTypes :: UProgram -> [(String, String)]
getTypes (Program cs) = map (\(Class name super _ _) -> (name, super)) cs

simpleCheck :: [(String, String)] -> Either String ()
simpleCheck types =
  case classes \\ nub classes of
    [] -> Right ()
    cs -> Left $ "Class redeclaration: " ++ show cs
  where
    classes = buildin ++ [x | (x, _) <- types]
    buildin = ["Object", "IO", "Int", "String", "SELF_TYPE"]

data TypeEnv = TypeEnv

getTypeEnv :: [(String, String)] -> TypeEnv
getTypeEnv = undefined
