module Hscool.CGen.Assembly where

data AssemblyCode = AssemblyCode [LabeledLine]

data LabeledLine = Label String CodeLine | Unlabeled CodeLine

data CodeLine = Comment String | Directive | Instruction

data Directive = undefined

data Instruction = undefined
