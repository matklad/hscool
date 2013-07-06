module Hscool.CGen.Preprocess where

import           Data.List                (findIndex)
import           Data.List.Utils          (replace)
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)
import           Hscool.CGen.Intermediate
import qualified Hscool.Types.AST         as A

preprocess :: A.TProgram -> Program
preprocess (A.Program aClasses) = let
        classMap = (M.fromList [(n, c) | c@(A.Class n _ _ _) <- aClasses] M.!)
        classes = map getClass aClasses
        methods = concatMap getMethods aClasses

        getAttrMap c@(A.Class _ super _ _) = let
                (_, attrs) = extractFeatures c
                m' = if super `elem` ["Object", "IO"]
                     then []
                     else getAttrMap $ classMap super
                m = [(n, A i)|(i, A.Attribute n _ _) <- zip [1 + length m'..] attrs]
            in
                m' ++ m

        getMethodMap c@(A.Class name super _ _) = let
                (meths, _) = extractFeatures c
                m' = if super `elem` ["Object", "IO"]
                     then [] -- it's a lie!
                     else getMethodMap $ classMap super
                m = [(n, concat [name, ".", n])|(A.Method n _ _ _) <- meths]
                aux ls (n, v) = case lookup n ls of
                    Nothing -> ls ++ [(n, v)]
                    Just v' -> replace [(n, v')] [(n, v)] ls
            in
                foldl aux m' m

        getClass c@(A.Class name super _ _) = let
                attrM = getAttrMap c
                methodM = getMethodMap c
            in
                Class name super (length attrM) (map snd methodM)

        getMethods c@(A.Class name _ _ _) = let
                (meths, _) = extractFeatures c
                aux (A.Method n formals _ e) = let
                        pMap = M.fromList [(p, P i) | (i, A.Formal p _) <- zip [1..] formals]
                        objMap = pMap `M.union` M.fromList  (getAttrMap c)
                        (nLoc, e') = prepExpr objMap 1 e
                    in
                        Method (concat [name, ".", n]) (length formals) nLoc e'
            in
                map aux meths

        prepExpr objMap nLoc (A.Expr t inner) = case inner of
                A.Assign s e -> let
                        (nLoc', e') = prepExpr objMap nLoc e
                    in
                        (nLoc', Assign (objMap M.! s) e')
                A.Dispatch e s es -> let
                        ti = fromMaybe
                                (error $ "unkown method " ++ s)
                                (findIndex (\(n, _) -> n == s) (getMethodMap . classMap $ t))
                        (nLoc', e') = prepExpr objMap nLoc e
                        (nLocs, es') = unzip . map (prepExpr objMap nLoc) $ es
                    in
                        (maximum $ nLoc' : nLocs, Dispatch e' ti es')
                A.StaticDispatch e tt s es -> let
                        ti = fromMaybe
                                (error $ "unkown method " ++ s)
                                (findIndex (\(n, _) -> n == s) (getMethodMap . classMap $ tt))
                        (nLoc', e') = prepExpr objMap nLoc e
                        (nLocs, es') = unzip . map (prepExpr objMap nLoc) $ es
                    in
                        (maximum $ nLoc' : nLocs, StaticDispatch e' (tt, ti) es')
                A.Cond e1 e2 e3 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                        (nLoc3, e3') = prepExpr objMap nLoc e3
                    in
                        (maximum [nLoc1, nLoc2, nLoc3], Cond e1' e2' e3')
                A.Loop e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                    in
                        (maximum [nLoc1, nLoc2], Loop e1' e2')
                A.TypeCase _ _ -> error "don't now how to deal with branches yet =("
                A.Block es -> let
                        (nLocs, es') = unzip . map (prepExpr objMap nLoc) $ es
                    in
                        (maximum nLocs, Block es')
                A.Let s _ e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        nLoc' = succ nLoc
                        objMap' = M.insert s (L nLoc') objMap
                        (nLoc2, e2') = prepExpr objMap' nLoc' e2
                    in
                        (maximum [nLoc1, nLoc2], Block [Assign (L nLoc') e1', e2'])
                A.Add e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                    in
                        (maximum [nLoc1, nLoc2], Add e1' e2')
                A.Minus e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                    in
                        (maximum [nLoc1, nLoc2], Minus e1' e2')

                A.Mul e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                    in
                        (maximum [nLoc1, nLoc2], Mul e1' e2')

                A.Div e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                    in
                        (maximum [nLoc1, nLoc2], Div e1' e2')

                A.Neg e -> let
                        (nLoc', e') = prepExpr objMap nLoc e
                    in
                        (nLoc', Neg e')
                A.Le e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                    in
                        (maximum [nLoc1, nLoc2], Le e1' e2')
                A.Eq e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                    in
                        (maximum [nLoc1, nLoc2], Eq e1' e2')
                A.Leq e1 e2 -> let
                        (nLoc1, e1') = prepExpr objMap nLoc e1
                        (nLoc2, e2') = prepExpr objMap nLoc e2
                    in
                        (maximum [nLoc1, nLoc2], Leq e1' e2')
                A.Comp e -> let
                        (nLoc', e') = prepExpr objMap nLoc e
                    in
                        (nLoc', Comp e')
                A.IntConst s -> (nLoc, IntConst s)
                A.StringConst s -> (nLoc, StringConst s)
                A.BoolConst b -> (nLoc, BoolConst b)
                A.New s -> (nLoc, New s)
                A.IsVoid e -> let
                        (nLoc', e') = prepExpr objMap nLoc e
                    in
                        (nLoc', IsVoid e')

                A.NoExpr -> error "WAT?! NO EXPR!!"
                A.Object s -> (nLoc, Object $    objMap M.! s)
    in
        Program classes methods


extractFeatures :: A.TClass -> ([A.TFeature], [A.TFeature])
extractFeatures (A.Class _ _ features _) = let
        methods = [x | x@(A.Method {}) <- features]
        attrs = [x | x@(A.Attribute {}) <- features]
    in
        (methods, attrs)
