module Main where
import Hscool.Types.AST
import qualified Data.ByteString as B

main = do
  input <- B.getContents
  let program =  parseUProgram input
  putStr $ show program
