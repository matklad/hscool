module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Hscool.Lexer.Base as L

main :: IO ()
main = do
    [fileName] <- getArgs
    contents <- readFile fileName
    printf "#name \"%s\"\n" fileName
    forM_ (L.alexScanTokens contents) $ \(c, t) ->
        printf "#%d %s\n" c (show t)
