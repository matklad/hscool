module Hscool.CGen.Assembly where
import Data.Monoid

import Text.Printf(printf)

class Code a where
    toAsm :: a -> AssemblyCode
    (|>) :: a -> a -> AssemblyCode
    a |> b = toAsm a `mappend` toAsm b


data AssemblyCode = AssemblyCode [CodeLine]

instance Monoid AssemblyCode where
    mempty = AssemblyCode []
    AssemblyCode a `mappend` AssemblyCode b = AssemblyCode (a `mappend` b)

instance Code AssemblyCode where
    toAsm = id

data CodeLine = Comment String
              | Label String
              | Ascii String
              | Asciiz String
              | Word Int
              | Wordl String
              | Data
              | Text
              | Global String
              | Jr String

instance Code CodeLine where
    toAsm l = AssemblyCode [l]

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
        (Comment s) -> "# " ++ s
        (Label s) -> s ++ ":"
        (Ascii s) -> ".ascii " ++ s
        (Asciiz s) -> ".asciiz " ++ s
        (Word i) -> printf ".word %d" i
        (Wordl s) -> ".word " ++ s
        Data -> ".data"
        Text -> ".text"
        (Global s) -> ".globl " ++ s
        Jr s -> printf "j %s" s
