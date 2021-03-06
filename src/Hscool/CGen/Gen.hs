module Hscool.CGen.Gen where

import Hscool.CGen.Assembly
import Hscool.CGen.Intermediate
import Hscool.CGen.Preprocess(getIntLabel, getStringLabel, getBoolLabel, voidLabel)
import Debug.Trace(trace)
import Control.Monad.State
import Control.Applicative((<$>))
import Data.List(intersperse, findIndex)
import Data.Char(ord)
import qualified Data.Map as M

cgen :: Program -> AssemblyCode
cgen (Program classes meths intConsts strConsts) = let
        protos = map genObjectProto classes
        funcs = map (genFunc classes) meths
        tables = map makeDispTable classes
            |> classNameTab classes
        intTag = findTag classes "Int"
        strTag = findTag classes "String"
        boolTag = findTag classes "Bool"
        tags = Label lIntTag
            |> Word intTag
            |> Label lStringTag
            |> Word strTag
            |> Label lBoolTag
            |> Word boolTag
        consts = map (makeIntConst intTag) intConsts
            |> map (makeStrConst strTag) strConsts
            |> map (makeBoolConst boolTag) [False, True]
        voidObj = Label voidLabel |> Word 0
    in
           Data
        |> globalLabels
        |> tags
        |> memMGR
        |> Comment "PROTOS"
        |> protos
        |> voidObj
        |> Comment "CONSTS"
        |> consts
        |> Comment "Tables"
        |> tables
        |> Comment "THE CODEZ"
        |> Label lHeapStart
        |> Word 0
        |> Text
        |> funcs

globalLabels :: AssemblyCode
globalLabels = Global lMainProtObj
    |> Global lMainInit
    |> Global lMainMain
    |> Global lIntProtObj
    |> Global lIntInit
    |> Global lStringProtObj
    |> Global lStringInit
    |> Global lIntTag
    |> Global lBoolTag
    |> Global lStringTag
    |> Global lBoolConst0
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

genObjectProto :: Class -> AssemblyCode
genObjectProto (Class tag name _ n_atts _) =
    case () of _
                | name == "Int" -> intProto tag
                | name == "Bool" -> boolProto tag
                | name == "String" -> strProto tag
                | otherwise -> gcTag
                            |> Label (protLabel name)
                            |> Word tag
                            |> Word (3 + n_atts)
                            |> Wordl (name ++ "_dispTab")
                            |> [Word 0 | _ <- [1..n_atts]]

protLabel :: String -> String
protLabel name = name ++ "_protObj"
initLabel :: String -> String
initLabel name = name ++ "_init"

intProto :: Int -> AssemblyCode
intProto tag = gcTag
    |> Label lIntProtObj
    |> Word tag
    |> Word 4
    |> Wordl lIntDispTab
    |> Word 0

strProto :: Int -> AssemblyCode
strProto tag = gcTag
    |> Label lStringProtObj
    |> Word tag
    |> Word 5
    |> Wordl lStringDispTab
    |> Wordl lIntProtObj
    |> Asciiz ""

boolProto :: Int -> AssemblyCode
boolProto tag = gcTag
    |> Label lBoolProtObj
    |> Word tag
    |> Word 4
    |> Wordl lBoolDispTab
    |> Word 0

makeIntConst :: Int -> String -> AssemblyCode
makeIntConst tag s = gcTag
    |> Label (getIntLabel s)
    |> Word tag
    |> Word 4
    |> Wordl lIntDispTab
    |> Word (read s)

makeStrConst :: Int -> String -> AssemblyCode
makeStrConst tag s =
    let
        aux x = case findIndex (=='\\') x of
            Nothing -> [Asciiz x]
            Just i -> let (h, _:t) = splitAt i x in
                   Ascii h
                |> Byte (ord '\\')
                |> aux t
    in
            gcTag
        |> Label (getStringLabel s)
        |> Word tag
        |> Word (5 + (length s `div` 4))
        |> Wordl lStringDispTab
        |> Wordl (getIntLabel . show . length $ s)
        |> aux s

makeBoolConst :: Int -> Bool -> AssemblyCode
makeBoolConst tag b = gcTag
    |> Label (getBoolLabel b)
    |> Word tag
    |> Word 4
    |> Wordl lBoolDispTab
    |> Word (if b then 1 else 0)

makeDispTable :: Class -> AssemblyCode
makeDispTable (Class _ name _ _ meths) = Label (name ++ "_dispTab")
    |> map Wordl meths

classNameTab :: [Class] -> AssemblyCode
classNameTab classes = Label lClassNameTab
    |> [ Wordl $ getStringLabel n| (Class _ n _ _ _) <- classes]

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
genFunc :: [Class] -> Method -> AssemblyCode
genFunc cs (Method name nP nL e) = let
        frameSize = (nP + nL + 1) * 4
        extendSize = (nL + 1) * 4
        labels = [name ++ "_lable_" ++ show i | i <- [1..]]
        ec = evalState (genExpr e) (labels, nP, cs)
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

type LState = State ([String], Int, [Class])

getLabels :: LState [String]
getLabels = do
    (labels, _, _) <- get
    return labels

putLabels :: [String] -> LState ()
putLabels labels = do
    (_, nP, cs) <- get
    put (labels, nP, cs)

getLocOff :: Int -> LState Int
getLocOff i = do
    (_, nP, _) <- get
    return $ (i + nP + 1) * 4

getClasses :: LState [Class]
getClasses = do
    (_, _, cs) <- get
    return cs

attrOff :: Int -> Int
attrOff i = (i + 3) * 4

genExpr :: Expr -> LState AssemblyCode
genExpr expr = case expr of
    Object S -> return $ push ra0
    Object (C s) -> return $ pushl s
    Object (P i) -> return $ Lw rt0 (-4 * i) rfp
        |> push rt0
    Object (A i) -> return $ Lw rt0 (attrOff i) ra0
        |> push rt0
    Object (L i) -> do
        off <- getLocOff i
        return $ Lw rt0 (-off) rfp
            |> push rt0
    Assign S _ -> error "assigning to self!o_O"
    Assign (C _) _ -> error "assigning to const!O_o"
    Assign (P i) e -> do
        ec <- genExpr e
        return $ ec
            |> Lw rt0 4 rsp
            |> Sw rt0 (-4 * i) rfp
    Assign (A i) e -> do
        ec <- genExpr e
        return $ ec
            |> Lw rt0 4 rsp
            |> Sw rt0 (attrOff i) ra0
    Assign (L i) e -> do
        ec <- genExpr e
        off <- getLocOff i
        return $ ec
            |> Lw rt0 4 rsp
            |> Sw rt0 (-off) rfp

    Dispatch e i es -> do
        ec <- genExpr e
        esc <- mapM genExpr es
        abort <- abortVoid lDispatchAbort
        return $ preCall
            |> ec
            |> esc
            |> Lw ra0 (4 * (1 + length es)) rsp
            |> abort
            |> Lw rt0 dispOff ra0
            |> Lw rt0 (i * 4) rt0
            |> Jalr rt0
            |> postCall
    StaticDispatch e (cls, i) es -> do
        ec <- genExpr e
        esc <- mapM genExpr es
        abort <- abortVoid lDispatchAbort
        return $ preCall
            |> ec
            |> esc
            |> Lw ra0 (4 * (1 + length es)) rsp
            |> abort
            |> La rt0 (cls ++ "_dispTab")
            |> Lw rt0 (i * 4) rt0
            |> Jalr rt0
            |> postCall
    Minus e1 e2 -> arith Subu e1 e2
    Add e1 e2 -> arith Addu e1 e2
    Mul e1 e2 -> arith Mulu e1 e2
    Div e1 e2 -> arith Divu e1 e2
    Neg e -> do
        ec <- genExpr e
        return $ push ra0
            |> ec
            |> La ra0 lIntProtObj
            |> Jal lObjectCopy
            |> loadAttr rt0
            |> Negu rt0 rt0
            |> Sw rt0 attr1Off ra0
            |> swapra0
    Comp e -> do
        ec <- genExpr e
        return $ push ra0
            |> ec
            |> La ra0 lBoolProtObj
            |> Jal lObjectCopy
            |> loadAttr rt0
            |> Xori rt0 1
            |> Sw rt0 attr1Off ra0
            |> swapra0
    Eq e1 e2 -> do
        l1:l2:ls <- getLabels
        putLabels ls
        e1c <- genExpr e1
        e2c <- genExpr e2
        return $ e1c
            |> e2c
            |> pop rt2
            |> pop rt1
            |> Beq rt1 rt2 l1
            |> push ra0
            |> La ra0 lBoolConst1
            |> La ra1 lBoolConst0
            |> Jal lEqualityTest
            |> swapra0
            |> J l2
            |> Label l1
            |> La rt0 lBoolConst1
            |> push rt0
            |> Label l2
    Le e1 e2 -> comp Blt e1 e2
    Leq e1 e2 -> comp Ble e1 e2

    Cond e1 e2 e3 -> do
        l1:l2:ls <- getLabels
        putLabels ls
        [e1c, e2c, e3c] <- mapM genExpr [e1, e2, e3]
        return $ e1c
            |> pop rt0
            |> Lw rt0 attr1Off rt0
            |> Beqz rt0 l1
            |> e2c
            |> J l2
            |> Label l1
            |> e3c
            |> Label l2
    Block es -> do
        ecs <- mapM genExpr es
        return .toAsm $ intersperse popn ecs
    New s -> return $ push ra0
        |> push rfp
        |> La ra0 (protLabel s)
        |> Jal lObjectCopy
        |> Jal (initLabel s)
        |> pop rfp
        |> swapra0

    IsVoid e -> do
        ec <- genExpr e
        l1:l2:ls <- getLabels
        putLabels ls
        return $ ec
            |> pop rt0
            |> Beqz rt0 l1
            |> pushl lBoolConst0
            |> J l2
            |> Label l1
            |> pushl lBoolConst1
            |> Label l2
    Loop e1 e2 -> do
        [e1c, e2c] <- mapM genExpr [e1, e2]
        l1:l2:ls <- getLabels
        putLabels ls
        return $ Label l1
            |> e1c
            |> pop rt0
            |> Lw rt0 attr1Off rt0
            |> Beqz rt0 l2
            |> e2c
            |> popn
            |> J l1
            |> Label l2
            |> pushl voidLabel

    TypeCase e bs -> genBranch e bs

    _ -> trace (show expr) $ return []


genBranch :: Expr -> [Branch] -> LState AssemblyCode
genBranch e bs = do
    cs <- getClasses
    let cm = M.fromList [(name, c)| c@(Class _ name _ _ _) <- cs]
    let bt = [t | Branch t _ <- bs]
    (end:bls, ls') <- splitAt (1 + length bs) <$> getLabels
    putLabels ls'
    abort <- abortVoid lCaseAbort2
    let aux cname =  (case findIndex (==cname) bt of
            Just i -> bls !! i
            Nothing -> if cname == "Object"
                       then lCaseAbort
                       else let (Class _ _ pname _ _) = (cm M.! cname)
                            in aux pname)
    es' <- mapM genExpr [ex |(Branch _ ex) <- bs]
    ec <- genExpr e
    let cases = [Label l
                 |> e'
                 |> J end
                 | (e', l) <- zip es' bls]
    let disp = [Li rt1 tag
                |> Beq rt0 rt1 (aux name)
                | Class tag name _ _ _ <- cs]
    return $ ec
        |> swapra0
        |> abort
        |> Lw rt0 tagOff ra0
        |> pop ra0
        |> disp
        |> cases
        |> Label end


preCall :: AssemblyCode
preCall = push rfp
    |> push ra0

postCall :: AssemblyCode
postCall = Move rt0 ra0
    |> popn
    |> pop ra0
    |> pop rfp
    |> push rt0

abortVoid :: String -> LState AssemblyCode
abortVoid abortLabel = do
    l1:ls <- getLabels
    putLabels ls
    return $ Bnez ra0 l1
        |> Li rt1 42 -- assume that error happened in the 42nd line of file named
        |> La ra0 lStringProtObj
        |> J abortLabel
        |> Label l1

arith :: (String -> String -> String -> CodeLine) -> Expr -> Expr -> LState AssemblyCode
arith op e1 e2 = do
    e1c <- genExpr e1
    e2c <- genExpr e2
    return $ push ra0
        |> e1c
        |> e2c
        |> La ra0 lIntProtObj
        |> Jal lObjectCopy
        |> loadAttr rt2
        |> loadAttr rt1
        |> op rt0 rt1 rt2
        |> Sw rt0 attr1Off ra0
        |> swapra0

comp :: (String -> String -> String -> CodeLine) -> Expr -> Expr -> LState AssemblyCode
comp op e1 e2 = do
    l1:l2:ls <- getLabels
    putLabels ls
    e1c <- genExpr e1
    e2c <- genExpr e2
    return $ e1c
        |> e2c
        |> loadAttr rt2
        |> loadAttr rt1
        |> op rt1 rt2 l1
        |> La rt0 lBoolConst0
        |> J l2
        |> Label l1
        |> La rt0 lBoolConst1
        |> Label l2
        |> push rt0

loadAttr :: String -> AssemblyCode
loadAttr r = pop r
    |> Lw r attr1Off r

swapra0 :: AssemblyCode
swapra0 = pop rt0
    |> push ra0
    |> Move ra0 rt0


tagOff :: Int
tagOff = 0
dispOff :: Int
dispOff = 8
attr1Off :: Int
attr1Off = 12
attr2Off :: Int
attr2Off = 16





















