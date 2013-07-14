{-# LANGUAGE FlexibleInstances #-}
module Hscool.CGen.Assembly where
import Data.Monoid
import Text.Printf(printf)

class Code a where
    toAsm :: a -> AssemblyCode

(|>) :: (Code x, Code y) => x -> y -> AssemblyCode
a |> b = toAsm a `mappend` toAsm b


type AssemblyCode = [CodeLine]

instance Code AssemblyCode where
    toAsm = id

instance Code [AssemblyCode] where
    toAsm  = mconcat

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
              | J String
              | Jalr String
              | Jal String Int
              | Beqz String String
              | Sw String Int String
              | Lw String Int String
              | Lwl String String
              | Addiu String String Int
              | La String String
              | Move String String

instance Code CodeLine where
    toAsm l = [l]

dump :: AssemblyCode -> String
dump codeLines = let
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
        (Ascii s) -> ".ascii " ++ show s
        (Asciiz s) -> ".asciiz " ++ show s
        (Word i) -> printf ".word %d" i
        (Wordl s) -> ".word " ++ s
        Data -> ".data"
        Text -> ".text"
        (Global s) -> ".globl " ++ s
        Jr r -> printf "jr %s" r
        J s -> printf "j %s" s
        Jalr r -> printf "jalr %s" r
        Jal s i -> printf "jal %s + %d" s i
        Beqz s l -> printf "beqz %s, %s" s l
        Sw s o d -> printf "sw %s, %d(%s)" s o d
        Lw s o d -> printf "lw %s, %d(%s)" s o d
        Lwl s l -> printf "lw %s, %s" s l
        Addiu r1 r2 i -> printf "addiu %s, %s, %d" r1 r2 i
        La d imm -> printf "la %s, %s" d imm
        Move dest source -> printf "move %s, %s" dest source


push :: String -> AssemblyCode
push r = Sw r 0 rsp |> Addiu rsp rsp (-4)

pushl :: String -> AssemblyCode
pushl l = La rt9 l |> push rt9

pop :: String -> AssemblyCode
pop r = Addiu rsp rsp 4 |> Lw r 0 rsp

popn :: AssemblyCode
popn = [Addiu rsp rsp 4]













































-- .global
lMainProtObj = "Main_protObj"
lMainInit = "Main_init"
lMainMain = "Main.main"
lIntProtObj = "Int_protObj"
lIntInit = "Int_init"
lStringProtObj = "String_protObj"
lStringInit = "String_init"
lIntTag = "_int_tag"
lBoolTag = "_bool_tag"
lStringTag = "_string_tag"
lClassNameTab = "class_nameTab"
lBoolConst0 = "bool_const0"
lBoolConst1 = "bool_const1"
lMemMgrInitializer = "_MemMgr_INITIALIZER"
lMemMgrCollector = "_MemMgr_COLLECTOR"
lHeapStart = "heap_start"
lMemMgrTest = "_MemMgr_TEST"


-- exported by RE
lObjectCopy = "Object.copy"
lObjectAbort = "Object.abort"
lObjectTypeName = "Object.type_name"
lIOOutString = "IO.out_string"
lIOOutInt = "IO.out_int"
lIOInString = "IO.in_string"
lIOInInt = "IO.in_int"
lStringLength = "String.length"
lStringLoncat = "String.concat"
lStringLubstr = "String.substr"
lEqualityTest = "equality_test"
lDispatchAbort = "_dispatch_abort"
lCaseAbort = "_case_abort"
lCaseAbort2 = "_case_abort2"
lNoGCInit = "_NoGC_Init"
lNoGCCollect = "_NoGC_Collect"

-- Tags
gcTag = Word (-1)

-- registers

ra0 = "$a0"
ra1 = "$a1"
rra = "$ra"
rsp = "$sp"
rfp = "$fp"
rt0 = "$t0"
rt1 = "$t1"
rt2 = "$t2"
rt9 = "$t9"
