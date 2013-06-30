module Main where
import           Control.Applicative     ((<$>))
import qualified Data.ByteString         as B
import           Hscool.Types.AST


main :: IO ()
main = do
    program <- parseTProgram <$> B.getContents
    putStr $ show program
