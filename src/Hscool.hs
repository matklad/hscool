module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Hscool.Lexer.Base as L

main :: IO ()
main = L.main
