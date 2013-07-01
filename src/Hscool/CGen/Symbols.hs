module Hscool.CGen.Symbols where

import Hscool.CGen.Assembly

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
objectTag = 0
intTag = 1
boolTag = 2
stringTag = 3

-- registers

ra0 = "$a0"
rra = "$ra"

-- stuff
lDoNothing = "_do_nothing"

genObjectProto label tag disp_tag attrs = AssemblyCode $ [
                                            gcTag
                                          , Label label
                                          , Word tag
                                          , Word size
                                          , Wordl disp_tag] ++ map Word attrs
    where
        size = 4 * (3 + length attrs)


intProto = genObjectProto lIntProtObj intTag "nothing" [0]


doNothing l = AssemblyCode [ Label l, Jr rra]
