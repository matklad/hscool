module Hscool.CGen.Gen where

import Hscool.CGen.Symbols
import Hscool.CGen.Assembly
import Hscool.Types.AST


cgen :: TProgram -> AssemblyCode
cgen = (\x -> dummy)


dummy = AssemblyCode [
          Data
        , Global lMainProtObj
        , Global lMainInit
        , Global lMainMain
        , Global "_MemMgr_INITIALIZER"
        , Global "_MemMgr_COLLECTOR"
        , Label "_MemMgr_INITIALIZER"
        , Wordl "_NoGC_Init"
        , Label "_MemMgr_COLLECTOR"
        , Wordl "_NoGC_Collect"
        , Label "heap_start"
        , Word 0
        , Label "_MemMgr_TEST"
        , Word 0
        ] |>
        (genObjectProto lMainProtObj intTag "nothing" [0]) |>
        AssemblyCode [Text] |>
        (doNothing lMainInit) |>
        (doNothing lMainMain)

