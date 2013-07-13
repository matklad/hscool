module Hscool.CGen.Gen where

import Hscool.CGen.Assembly
import Hscool.CGen.Intermediate
import Hscool.CGen.Preprocess(getIntLabel, getStringLabel, getBoolLabel)
import Debug.Trace(trace)


cgen :: Program -> AssemblyCode
cgen (Program classes meths intConsts strConsts) = let
        protos = map genObjectProto classes
        funcs = map genFunc meths
        tables = map makeDispTable classes
        intTag = findTag classes "Int"
        strTag = findTag classes "String"
        boolTag = findTag classes "Bool"
        tags = Label lIntTag
            |> Word intTag
            |> Label lStringTag
            |> Word strTag
            |> Label lBoolTag
            |> Word boolTag
        consts = map (makeIntConst strTag) intConsts
            |> map (makeStrConst intTag) strConsts
            |> map (makeBoolConst boolTag) [False, True]
    in
           Data
        |> globalLabels
        |> tags
        |> memMGR
        |> Comment "PROTOS"
        |> protos
        |> Comment "CONSTS"
        |> consts
        |> Comment "Tables"
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
    |> Word (3 + length s `div` 4)
    |> Wordl "String_dispTab"
    |> Word (length s)
    |> Asciiz s

makeBoolConst :: Int -> Bool -> AssemblyCode
makeBoolConst tag b = gcTag
    |> Label (getBoolLabel b)
    |> Word tag
    |> Word 4
    |> Wordl "Bool_dispTab"
    |> Word (if b then 1 else 0)

makeDispTable :: Class -> AssemblyCode
makeDispTable (Class _ name _ _ meths) = Label (name ++ "_dispTab")
    |> map Wordl meths

{- calling conventions
foo() {
    ...
 -> e.bar(e1, e2)
    ...
}

    ######################### 0xffffffff
    #        Static         #
    #########################
    #         Stack         # |
    #         ....          # v
    #         ....          #
    # foo fp                #
    # foo self              #
    # e                     #
    # e0                    # <- bar fp
    # e1                    #
    # return address        #
    # loc0                  #
    # loc1                  #
    #                       #
    #                       #
    #                       #
    .........................
    .........................
    #                       #
    #                       #
    #                       # ^
    #                       # |
    #        Heap           #
    ######################### 0x00000000

bar pops arguments

-}
genFunc :: Method -> AssemblyCode
genFunc (Method name nP nL e) = let
        frameSize = (nP + nL + 1) * 4
        extendSize = (nL + 1) * 4
    in
           Label name
        |> Sw rra 0 rsp
        |> Addiu rsp rsp (-extendSize)
        |> Addiu rfp rsp frameSize
        |> genExpr e
        |> Lw rra extendSize rsp
        |> Addiu rsp rsp frameSize
        |> Jr rra

findTag :: [Class] -> String -> Int
findTag cls s = let [Class tag _ _ _ _] = [c | c@(Class _ name _ _ _) <- cls, name == s]
    in tag

genExpr :: Expr -> AssemblyCode
genExpr expr = case expr of
    Object S -> push ra0
    Object (C s) -> pushl s
    Object (P i) -> Lw rt0 (-4 * i) rfp
        |> push rt0
    Dispatch e i es -> push rfp
        |> push ra0
        |> genExpr e
        |> map genExpr es
        |> Lw ra0 (4 * (1 + length es)) rsp
        |> Lw rt0 8 ra0
        |> Lw rt0 (i * 4) rt0
        |> Jalr rt0
        |> pop ra0
        |> pop ra0
        |> pop rfp
    _ -> trace (show expr) $ []


