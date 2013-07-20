module Hscool.CGen.Preprocess where

import           Control.Applicative      ((<$>))
import           Data.Char                (isAlphaNum)
import           Data.Hashable
import           Data.List                (findIndex, unzip4)
import           Data.List                (nub)
import           Data.List.Utils          (replace)
import qualified Data.Map                 as M
import           Data.Maybe               (catMaybes, fromMaybe)
import           Hscool.CGen.Intermediate
import qualified Hscool.Types.AST         as A
import           Text.Printf              (printf)


preprocess :: A.TProgram -> Program
preprocess (A.Program aClasses) = let
        aClasses' = buildin ++ aClasses
        classMap = (M.fromList [(n, c) | c@(A.Class n _ _ _) <- aClasses'] M.!)
        classes = zipWith (curry getClass) [0..] aClasses'
        (methods', ints', strings') = unzip3 . map getMethods $ aClasses'
        strings = nub $ concat ([n |(A.Class n _ _ _) <- aClasses']:strings')
        ints = nub $ concat (map (show.length) strings : ints')

        methods = filter
            (\(Method name _ _ _) ->  (name `notElem` ["String.length", "String.substr", "String.concat",
                "IO.in_int", "IO.in_string", "IO.out_int", "IO.out_string", "Object.copy",
                "Object.type_name", "Object.abort"]))
            (concat methods')

        getAttrMap c@(A.Class name super _ _) = let
                (_, attrs) = extractFeatures c
                m' = if name /= "Object"
                     then getAttrMap $ classMap super
                     else []
                m = [(n, A i)|(i, A.Attribute n _ _) <- zip [0 + length m'..] attrs]
            in
                m' ++ m

        getMethodMap c@(A.Class name super _ _) = let
                (meths, _) = extractFeatures c
                m' = if name /= "Object"
                     then getMethodMap $ classMap super
                     else []
                m = ("_init", name ++ "_init") : [(n, concat [name, ".", n])|(A.Method n _ _ _) <- meths]
                aux ls (n, v) = case lookup n ls of
                    Nothing -> ls ++ [(n, v)]
                    Just v' -> replace [(n, v')] [(n, v)] ls
            in
                foldl aux m' m

        getClass (tag, c@(A.Class name super _ _)) = let
                attrM = getAttrMap c
                methodM = getMethodMap c
            in
                Class tag name super (length attrM) (map snd methodM)

        getInit c@(A.Class name super _ _) = let
                (_, attrs) = extractFeatures c
                aux e = case e of
                    A.Expr _ A.NoExpr -> Nothing
                    _ -> Just e
                assignDefault (n, t) = (\e -> A.Expr t (A.Assign n (A.Expr t e))) <$>
                    case () of _
                                | t == "Int" ->
                                    Just $ A.IntConst "0"
                                | t == "String" ->
                                    Just $ A.StringConst ""
                                | t == "Bool" ->
                                    Just $ A.BoolConst False
                                | otherwise -> Nothing

                defaults = catMaybes . map assignDefault $ [(n, t) | (A.Attribute n t _) <- attrs]
                inits = (catMaybes [A.Expr t <$> (A.Assign n <$> aux e)| (A.Attribute n t e) <- attrs])
                        ++ [A.Expr "SELF_TYPE" $ A.Object "self"]

                dinits = defaults ++ inits
                superCall = A.Expr "Object" $ A.StaticDispatch (A.Expr "Object" (A.Object "self")) super "_init" []

                inits' = if name /= "Object"
                         then superCall : dinits
                         else dinits
            in
                A.Method "_init" [] "Object" $ A.Expr "Object" $ A.Block inits'

        getMethods c@(A.Class name _ _ _) = let
                (meths, _) = extractFeatures c
                meths' = getInit c : meths

                aux (A.Method n formals _ e) = let
                        pMap = M.fromList [(p, P i) | (i, A.Formal p _) <- zip [0..] formals]
                        objMap = pMap `M.union` M.fromList  (getAttrMap c)
                        (nLoc, e', is, ss) = prepExpr objMap 0 e
                        qualName = concat [name, if n == "_init" then "" else ".", n]
                    in
                        (Method qualName (length formals) nLoc e', is, ss)
                (pMeths, ints', strings') = unzip3 . map aux $ meths'
                ints = concat ints'
                strings = concat strings'

                prepExpr objMap nLoc (A.Expr _ inner) = case inner of
                        A.Assign s e -> let
                                (nLoc', e', is, ss) = prepExpr objMap nLoc e
                            in
                                (nLoc', Assign (objMap M.! s) e', is, ss)
                        A.Dispatch e@(A.Expr t _) s es -> let
                                t' = if t /= "SELF_TYPE"
                                     then t
                                     else name
                                ti = fromMaybe
                                        (error $ "unkown method " ++ s)
                                        (findIndex (\(n, _) -> n == s) (getMethodMap . classMap $ t'))
                                (nLoc', e', is, ss) = prepExpr objMap nLoc e
                                (nLocs, es', iss, sss) = unzip4 . map (prepExpr objMap nLoc) $ es
                            in
                                (maximum $ nLoc' : nLocs, Dispatch e' ti es', concat (is:iss), concat (ss:sss))
                        A.StaticDispatch e tt s es -> let
                                ti = fromMaybe
                                        (error $ "unkown method " ++ s)
                                        (findIndex (\(n, _) -> n == s) (getMethodMap . classMap $ tt))
                                (nLoc', e', is, ss) = prepExpr objMap nLoc e
                                (nLocs, es', iss, sss) = unzip4 . map (prepExpr objMap nLoc) $ es
                            in
                                (maximum $ nLoc' : nLocs, StaticDispatch e' (tt, ti) es', concat (is:iss), concat (ss:sss))
                        A.Cond e1 e2 e3 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                                (nLoc3, e3', is3, ss3) = prepExpr objMap nLoc e3
                            in
                                (maximum [nLoc1, nLoc2, nLoc3], Cond e1' e2' e3', concat [is1, is2, is3], concat [ss1, ss2, ss3])
                        A.Loop e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                            in
                                (maximum [nLoc1, nLoc2], Loop e1' e2', is1 ++ is2, ss1 ++ ss2)
                        A.TypeCase _ _ -> error "don't now how to deal with branches yet =("
                        A.Block es -> let
                                (nLocs, es', iss, sss) = unzip4 . map (prepExpr objMap nLoc) $ es
                            in
                                (maximum $ nLoc : nLocs, Block es', concat iss, concat sss)
                        A.Let s _ e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                nLoc' = succ nLoc
                                objMap' = M.insert s (L nLoc) objMap
                                (nLoc2, e2', is2, ss2) = prepExpr objMap' nLoc' e2
                            in
                                (maximum [nLoc1, nLoc2], Block [Assign (L nLoc) e1', e2'], is1 ++ is2, ss1 ++ ss2)
                        A.Add e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                            in
                                (maximum [nLoc1, nLoc2], Add e1' e2', is1 ++ is2, ss1 ++ ss2)
                        A.Minus e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                            in
                                (maximum [nLoc1, nLoc2], Minus e1' e2', is1 ++ is2, ss1 ++ ss2)

                        A.Mul e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                            in
                                (maximum [nLoc1, nLoc2], Mul e1' e2', is1 ++ is2, ss1 ++ ss2)

                        A.Div e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                            in
                                (maximum [nLoc1, nLoc2], Div e1' e2', is1 ++ is2, ss1 ++ ss2)

                        A.Neg e -> let
                                (nLoc', e', is, ss) = prepExpr objMap nLoc e
                            in
                                (nLoc', Neg e', is, ss)
                        A.Le e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                            in
                                (maximum [nLoc1, nLoc2], Le e1' e2', is1 ++ is2, ss1 ++ ss2)
                        A.Eq e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                            in
                                (maximum [nLoc1, nLoc2], Eq e1' e2', is1 ++ is2, ss1 ++ ss2)
                        A.Leq e1 e2 -> let
                                (nLoc1, e1', is1, ss1) = prepExpr objMap nLoc e1
                                (nLoc2, e2', is2, ss2) = prepExpr objMap nLoc e2
                            in
                                (maximum [nLoc1, nLoc2], Leq e1' e2', is1 ++ is2, ss1 ++ ss2)
                        A.Comp e -> let
                                (nLoc', e', is, ss) = prepExpr objMap nLoc e
                            in
                                (nLoc', Comp e', is, ss)
                        A.IntConst s -> (nLoc, Object (C $ getIntLabel s), [s], [])
                        A.StringConst s -> let s' = read s in
                            (nLoc, Object (C $ getStringLabel s'), [], [s'])
                        A.BoolConst b -> (nLoc, Object (C $ getBoolLabel b), [], [])
                        A.New s -> (nLoc, New s, [], [])
                        A.IsVoid e -> let
                                (nLoc', e', is, ss) = prepExpr objMap nLoc e
                            in
                                (nLoc', IsVoid e', is, ss)

                        A.NoExpr -> (nLoc, Block [], [], []) -- NoExpr happens =(
                        A.Object s -> (nLoc, Object $ if s /= "self" then objMap M.! s else S, [], [])

            in
                (pMeths, ints, strings)
    in
        Program classes methods ints strings


getIntLabel :: String -> String
getIntLabel s = "int_const_" ++ s

getStringLabel :: String -> String
getStringLabel s = let h = hash s `mod` 10000 in
    printf "str_const_%s_%d" (filter isAlphaNum s) h

getBoolLabel :: Bool -> String
getBoolLabel b = if b
    then "bool_const1"
    else "bool_const0"

extractFeatures :: A.TClass -> ([A.TFeature], [A.TFeature])
extractFeatures (A.Class _ _ features _) = let
        methods = [x | x@(A.Method {}) <- features]
        attrs = [x | x@(A.Attribute {}) <- features]
    in
        (methods, attrs)

ne :: A.TExpr
ne = A.Expr "Object" A.NoExpr

object :: A.TClass
object = A.Class "Object" "_"
    [ A.Method "abort" [] "Object" ne
    , A.Method "type_name" [] "String" ne
    , A.Method "copy" [] "SELF_TYPE" ne]
    "_"

isObject :: A.TClass -> Bool
isObject = (== object)

io :: A.TClass
io = A.Class "IO" "Object"
    [ A.Method "out_string" [A.Formal "x" "String"] "SELF_TYPE" ne
    , A.Method "out_int" [A.Formal "x" "Int"] "SELF_TYPE" ne
    , A.Method "in_string" [] "String" ne
    , A.Method "in_int" [] "Int" ne ]
    "_"

int :: A.TClass
int = A.Class "Int" "Object" [] "_"

string :: A.TClass
string = A.Class "String" "Object"
    [ A.Method "length" [] "Int" ne
    , A.Method "concat" [A.Formal "s" "String"] "String" ne
    , A.Method "substr" [A.Formal "i" "Int", A.Formal "l" "Int"] "String" ne]
    "_"

bool :: A.TClass
bool = A.Class "Bool" "Object" [] "_"

buildin :: [A.TClass]
buildin = [object, io, int, string, bool]

