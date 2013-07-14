module Hscool.CGen.Gen where

import Hscool.CGen.Assembly
import Hscool.CGen.Intermediate
import Hscool.CGen.Preprocess(getIntLabel, getStringLabel, getBoolLabel)
import Debug.Trace(trace)
import Control.Monad.State
import Data.List(intersperse)

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
a0 holds self on entry
a0 holds ret on exit
-}
genFunc :: Method -> AssemblyCode
genFunc (Method name nP nL e) = let
        frameSize = (nP + nL + 1) * 4
        extendSize = (nL + 1) * 4
        labels = [name ++ "_lable_" ++ show i | i <- [1..]]
        ec = evalState (genExpr e) labels
    in
           Label name
        |> Sw rra 0 rsp
        |> Addiu rsp rsp (-extendSize)
        |> Addiu rfp rsp frameSize
        |> ec
        |> pop ra0
        |> Lw rra extendSize rsp
        |> Addiu rsp rsp frameSize
        |> Jr rra

findTag :: [Class] -> String -> Int
findTag cls s = let [Class tag _ _ _ _] = [c | c@(Class _ name _ _ _) <- cls, name == s]
    in tag

type LState = State [String]

genExpr :: Expr -> LState AssemblyCode
genExpr expr = case expr of
    Object S -> return $ push ra0
    Object (C s) -> return $ pushl s
    Object (P i) -> return $ Lw rt0 (-4 * i) rfp
        |> push rt0
    Assign S _ -> error "assigning to self!o_O"
    Assign (C _) _ -> error "assigning to const!O_o"

    Dispatch e i es -> do
        ec <- genExpr e
        esc <- mapM genExpr es
        return $ preCall
            |> ec
            |> esc
            |> Lw ra0 (4 * (1 + length es)) rsp
            |> Lw rt0 8 ra0
            |> Lw rt0 (i * 4) rt0
            |> Jalr rt0
            |> postCall
    StaticDispatch e (cls, i) es -> do
        ec <- genExpr e
        esc <- mapM genExpr es
        return $ preCall
            |> ec
            |> esc
            |> La rt0 (cls ++ "_dispTab")
            |> Lw rt0 (i * 4) rt0
            |> Jalr rt0
            |> postCall
    Eq e1 e2 -> do
        e1c <- genExpr e1
        e2c <- genExpr e2
        return $ e1c
            |> pop rt1
            |> e2c
            |> pop rt2
            |> push ra0
            |> Lwl ra0 lBoolConst1
            |> Lwl ra1 lBoolConst0
            |> J lEqualityTest
            |> pop rt0
            |> push ra0
            |> Move ra0 rt0
    Cond e1 e2 e3 -> do
        l1:l2:ls <- get
        put ls
        [e1c, e2c, e3c] <- mapM genExpr [e1, e2, e3]
        return $ e1c
            |> pop rt0
            |> Lw rt0 12 rt0
            |> Beqz rt0 l1
            |> e2c
            |> J l2
            |> Label l1
            |> e3c
            |> Label l2
    Block es -> do
        ecs <- mapM genExpr es
        return .toAsm $ intersperse popn ecs
    _ -> trace (show expr) $ return []

preCall :: AssemblyCode
preCall = push rfp
    |> push ra0

postCall :: AssemblyCode
postCall = Move rt0 ra0
    |> popn
    |> pop ra0
    |> pop rfp
    |> push rt0


