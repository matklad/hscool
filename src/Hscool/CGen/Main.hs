module Main where
import           Control.Applicative     ((<$>))
import qualified Data.ByteString         as B
import           Hscool.Types.AST
import Hscool.CGen.Gen
import Hscool.CGen.Preprocess
import Hscool.CGen.Assembly(dump)


main :: IO ()
main = do
    program <- parseTProgram <$> B.getContents
    let program' = preprocess program
    let code = cgen program'
    putStrLn . dump $ code
