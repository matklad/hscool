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
        , Global lMemMgrInitializer
        , Global lMemMgrCollector
        , Global lHeapStart
        , Global lMemMgrTest
        , Label lMemMgrInitializer
        , Wordl lNoGCInit
        , Label lMemMgrCollector
        , Wordl lNoGCCollect
        , Label lHeapStart
        , Word 0
        , Label lMemMgrTest
        , Word 0
        ] |>
        (genObjectProto lMainProtObj intTag "nothing" [0]) |>
        AssemblyCode [Text] |>
        (doNothing lMainInit) |>
        (doNothing lMainMain)

