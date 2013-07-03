module Hscool.CGen.Preprocess where

import           Hscool.Types.AST
import qualified Data.Map as M

preprocess :: TProgram -> TProgram
preprocess = alphaConvert . makeInits


makeInits :: TProgram -> TProgram
makeInits (Program classes) = let
        makeInit :: TClass -> TClass
        makeInit c@(Class name super _ file) = let
                aux attrs = let (as, ex) = aux $ tail attrs in case attrs of
                    [] -> ([], [])
                    a@(Attribute _ _ (Expr "_no_type" NoExpr)):_ -> (a:as, ex)
                    Attribute n t e :_ -> (Attribute n t (Expr "_no_type" NoExpr):as,
                                            Expr t (Assign n e):ex)
                (methods, attributes) = extractFeatures c
                (newAttrs, ini) = aux attributes
            in
                Class name super (newAttrs ++ (Method "_init" [] "Object" (Expr "Object" $ Block ini):methods)) file

    in
        Program $ map makeInit classes

extractFeatures :: TClass -> ([TFeature], [TFeature])
extractFeatures (Class _ _ features _) = let
        methods = [m | m@(Method {}) <- features]
        attributes = [a | a@(Attribute {}) <- features]
    in
        (methods, attributes)


alphaConvert :: TProgram -> TProgram
alphaConvert (Program classes) = let
        m = M.fromList [(name, c) | c@(Class name _ _ _) <- classes]
        rename attrs start = [Attribute ('a' : show i) t e|(i, Attribute _ t e) <- zip [start..] attrs]
        startInd (Class _ super _ _) = if super `elem` ["Object", "IO"]
            then 1
            else let s = (m M.! super) in (length.snd.extractFeatures $ s) + startInd s
        convertAttributes c@(Class name super _ file) = let
                (methods, attributes) = extractFeatures c
            in
                Class name super (rename attributes (startInd c) ++ methods) file

        convertedClasses = map convertAttributes classes
    in
        Program convertedClasses
