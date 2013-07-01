module Hscool.CGen.Preprocess where

import           Hscool.Types.AST

preprocess :: TProgram -> TProgram
preprocess = alphaConvert . makeInits

alphaConvert :: TProgram -> TProgram
alphaConvert = id

makeInits :: TProgram -> TProgram
makeInits (Program classes) = let
        makeInit :: TClass -> TClass
        makeInit (Class name super features file) = let
                methods = [m | m@(Method {}) <- features]
                attributes = [a | a@(Attribute {}) <- features]
                aux attrs = let (as, ex) = aux $ tail attrs in case attrs of
                    [] -> ([], [])
                    a@(Attribute _ _ (Expr "_no_type" NoExpr)):_ -> (a:as, ex)
                    (Attribute n t e):_ -> ((Attribute n t (Expr "_no_type" NoExpr)):as,
                                            (Expr t (Assign n e):ex))
                (newAttrs, ini) = aux attributes
            in
                Class name super (newAttrs ++ (Method "_init" [] "Object" (Expr "Object" $ Block ini):methods)) file

    in
        Program $ map makeInit classes

