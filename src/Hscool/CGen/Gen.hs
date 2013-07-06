module Hscool.CGen.Gen where

import Hscool.CGen.Assembly
import Hscool.CGen.Intermediate


cgen :: Program -> AssemblyCode
cgen (Program classes meths) = let
        protos = map genObjectProto classes
        funcs = map genFunc meths
        tables = Word 0
    in
           Data
        |> globalLabels
        |> memMGR
        |> protos
        |> tables
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


genFunc :: Method -> AssemblyCode
genFunc (Method name n_p n_l e) = Label name
    |> Addiu rsp rsp (-4)
    |> Sw rra 4 rsp
    |> Lw rra 4 rsp
    |> Addiu rsp rsp 8
    |> Jr rra
