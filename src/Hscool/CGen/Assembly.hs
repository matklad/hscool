module Hscool.CGen.Assembly where

import Text.Printf(printf)

data AssemblyCode = AssemblyCode [CodeLine]

data CodeLine = Comment String
              | Label String
              | Ascii String
              | Asciiz String
              | Word Int
              | Wordl String
              | Data

instance Show AssemblyCode where
    show (AssemblyCode codeLines) = let
            aux x = case x of
                (Comment {}) -> show x
                (Label {}) -> show x
                _ -> "    " ++ (show x)
        in
            unlines . (map aux) $ codeLines

instance Show CodeLine where
    show line = case line of
        (Comment c) -> "# " ++ c
        (Label l) -> l ++ ":"
        (Ascii s) -> ".ascii " ++ s
        (Asciiz s) -> ".asciiz " ++ s
        (Word i) -> printf ".word %d" i
        (Wordl l) -> printf ".word %s" l
        Data -> ".data"

gcTag :: CodeLine
gcTag = Word (-1)
