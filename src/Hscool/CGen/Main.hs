module Main where
import           Control.Applicative     ((<$>))
import qualified Data.ByteString         as B
import           Hscool.Types.AST
import Hscool.CGen.Gen


main :: IO ()
main = do
    program <- parseTProgram <$> B.getContents
    let code = cgen program
    print code
