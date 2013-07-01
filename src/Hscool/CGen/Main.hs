module Main where
import           Control.Applicative     ((<$>))
import qualified Data.ByteString         as B
import           Hscool.Types.AST
import Hscool.CGen.Gen
import Hscool.CGen.Preprocess


main :: IO ()
main = do
    program <- parseTProgram <$> B.getContents
    print $ preprocess program
