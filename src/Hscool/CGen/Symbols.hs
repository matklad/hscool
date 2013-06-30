module Hscool.CGen.Symbols where

import Hscool.CGen.Assembly

-- .global
lMainProtObj = "Main_protObj"
lMainInit = "Main_init"
lMainMain = "main.Main"
lInt_protObj = "Int_protObj"
lInt_init = "Int_init"
lString_protObj = "String_protObj"
lString_init = "String_init"
lInt_tag = "_int_tag"
lBool_tag = "_bool_tag"
lString_tag = ")string_tag"
lClass_nameTab = "class_nameTab"
lBool_const0 = "bool_const0"

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

gcTag = Word (-1)
