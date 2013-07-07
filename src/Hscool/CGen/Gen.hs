module Hscool.CGen.Gen where

import Hscool.CGen.Assembly
import Hscool.CGen.Intermediate
import Hscool.CGen.Preprocess(getIntLabel, getStringLabel, getBoolLabel)


cgen :: Program -> AssemblyCode
cgen (Program classes meths intConsts strConsts) = let
        protos = map genObjectProto classes
        funcs = map genFunc meths
        tables = Word 0
        intTag = findTag classes "Int"
        strTag = findTag classes "String"
        boolTag = findTag classes "Bool"
        consts = (map (makeIntConst strTag) intConsts)
            |> (map (makeStrConst intTag) strConsts)
            |> (map (makeBoolConst boolTag) [False, True])
    in
           Data
        |> globalLabels
        |> memMGR
        |> Comment "PROTOS"
        |> protos
        |> Comment "CONSTS"
        |> consts
        |> tables
        |> Comment "THE CODEZ"
        |> Text
        |> funcs

globalLabels :: AssemblyCode
globalLabels = Global lMainProtObj
    |> Global lMainInit
    |> Global lMainMain
    |> Global lMemMgrInitializer
    |> Global lMemMgrCollector
    |> Global lHeapStart
    |> Global lMemMgrTest

memMGR :: AssemblyCode
memMGR = Label lMemMgrCollector
    |> Wordl lNoGCCollect
    |> Label lMemMgrInitializer
    |> Wordl lNoGCInit
    |> Label lMemMgrTest
    |> Word 0
    |> Label lHeapStart
    |> Word 0

genObjectProto :: Class -> AssemblyCode
genObjectProto (Class tag name _ n_atts _) = gcTag
    |> Label (name ++ "_protObj")
    |> Word tag
    |> Word (3 + n_atts)
    |> Wordl (name ++ "_dispTab")
    |> [Word 0 | _ <- [1..n_atts]]

makeIntConst :: Int -> String -> AssemblyCode
makeIntConst tag s = gcTag
    |> Label (getIntLabel s)
    |> Word tag
    |> Word 4
    |> Wordl "Int_dispTab"
    |> Word (read s)

makeStrConst :: Int -> String -> AssemblyCode
makeStrConst tag s = gcTag
    |> Label (getStringLabel s)
    |> Word tag
    |> Word (3 + (length s) `div` 4)
    |> Wordl "String_dispTab"
    |> Asciiz s

makeBoolConst :: Int -> Bool -> AssemblyCode
makeBoolConst tag b = gcTag
    |> Label (getBoolLabel b)
    |> Word tag
    |> Word 4
    |> Wordl "Bool_dispTab"
    |> Word (if b then 1 else 0)

genFunc :: Method -> AssemblyCode
genFunc (Method name nP nL e) = let
        frameSize = (nL + 1) * 4
        popSize = (max 0 (nP - 1)) * 4
    in
           Label name
        |> Addiu rsp rsp (-frameSize)
        |> Sw rra frameSize rsp
        |> Lw rra frameSize rsp
        |> Addiu rsp rsp popSize
        |> Jr rra

findTag :: [Class] -> String -> Int
findTag cls s = let [Class tag _ _ _ _] = [c | c@(Class _ name _ _ _) <- cls, name == s]
    in tag

