module Hscool.Semant.TypeCheck(typeCheck) where
import           Control.Applicative     (pure, (<$>), (<*>))
import Control.Monad
import           Data.List               (nub)
import qualified Data.Map                as M
import qualified Hscool.Semant.GlobalEnv as G
import           Hscool.Types.AST

typeCheck :: G.GlobalEnv -> UProgram -> Either String TProgram
typeCheck env (Program classes)= Program <$>  mapM (typeCheckClass env) classes

typeCheckClass :: G.GlobalEnv -> UClass -> Either String TClass
typeCheckClass env (Class name type_ features file) =
        Class name type_
        <$> mapM (typeCheckFeature env name) features
        <*> pure file

typeCheckFeature :: G.GlobalEnv -> String -> UFeature -> Either String TFeature
typeCheckFeature env cname feature = case feature of
        Method name formals type_ expr -> do
            objects <- addFormals (G.getAttrs env cname) formals
            let context = (env, cname, objects)
            expr'@(Expr t _) <- typeCheckExpr context expr
            if G.isSubtype env t type_
                then return $ Method name formals type_ expr'
                else Left $ "Method body has wrong return type: " ++ name

        Attribute name type_ expr -> do
            let context = (env, cname, G.getAttrs env cname)
            expr'@(Expr t _) <- typeCheckExpr context expr
            if G.isSubtype env t type_
                then return $ Attribute name type_ expr'
                else Left $ "Attribute initialization has wrong type: " ++ name
    where
        addFormals objects formals = if formals == nub formals
            then pure $ M.union
                        (M.fromList [(name, type_)| (Formal name type_) <- formals])
                        objects
            else Left "formal parameters with duplicate names!"

typeCheckExpr :: Context -> UExpr -> Either String TExpr
typeCheckExpr context expression = do
    (_, expression') <- aux context expression
    return expression'
    where
        aux':: Context -> Expr' NT -> Either String (String, Expr' String)
        aux' cont expr' = case expr' of
            Assign o e -> do
                ot <- getType cont o
                (et, e') <- aux cont e
                if isSubtype cont et ot
                    then return (et, Assign o e')
                    else Left "assignment type mismatch"

            Dispatch e m actuals -> do
                (et, e') <- aux cont e
                (ats, as') <- unzip <$> mapM (aux cont) actuals
                let et' = if et == "SELF_TYPE"
                          then currentClass cont
                          else et
                (rt:fs) <- getMethod cont et' m
                unless (all (uncurry $ isSubtype cont) (zip ats fs))
                    $ Left "method called with wrong arguments"
                let rt' = if rt == "SELF_TYPE"
                          then et
                          else rt
                return (rt', Dispatch e' m as')

            StaticDispatch e type_ m actuals -> do
                (et, e') <- aux cont e
                (ats, as') <- unzip <$> mapM (aux cont) actuals
                unless (isSubtype cont et type_)
                    $ Left "static dispatch wrong type"
                (rt:fs) <- getMethod cont type_ m
                unless (all (uncurry $ isSubtype cont) (zip ats fs))
                    $ Left "method called with wrong arguments"

                let rt' = if rt == "SELF_TYPE"
                          then et
                          else rt
                return (rt', StaticDispatch e' type_ m as')

            Cond e1 e2 e3 -> do
                (e1t, e1') <- aux cont e1
                unless (e1t == "Bool")
                    $ Left "if condition is not boolean"
                (e2t, e2') <- aux cont e2
                (e3t, e3') <- aux cont e3
                let s = superType cont [e2t, e3t]
                return (s, Cond e1' e2' e3')

            Loop e1 e2 -> do
                (e1t, e1') <- aux cont e1
                unless (e1t == "Bool")
                    $ Left "While condition is not boolean"
                (_, e2') <- aux cont e2
                return ("Object", Loop e1' e2')

            TypeCase e bs -> let
                    checkBranch (Branch name type_ be) = do
                        let cont' = addToContext cont name type_
                        (bet, be') <- aux cont' be
                        return (type_, bet, Branch name type_ be')
                in do
                    (_, e') <- aux cont e
                    (ts, ts', bs') <- unzip3 <$> mapM checkBranch bs
                    unless (nub ts == ts)
                           $ Left "duplicate branches in case"
                    return (superType cont ts', TypeCase e' bs')

            Block es -> do
                (est, es') <- unzip <$> mapM (aux cont) es
                return (last est, Block es')

            Let name type_ e1 e2 -> do
                (e1t, e1') <- aux cont e1
                unless (isSubtype cont e1t type_)
                       $ Left "incompatible types in let statement"
                let cont' = addToContext cont name type_
                (e2t, e2') <- aux cont' e2
                return (e2t, Let name type_ e1' e2')

            Add e1 e2 -> arith cont Add e1 e2

            Minus e1 e2-> arith cont Minus e1 e2

            Mul e1 e2 -> arith cont Mul e1 e2

            Div e1 e2 -> arith cont Div e1 e2

            Neg e -> do
                (et, e') <- aux cont e
                unless (et == "Bool")
                    $ Left "not argument is not boolean"
                return ("Bool", Neg e')


            Le e1 e2 -> comp cont Le e1 e2

            Eq e1 e2 -> do
                (e1t, e1') <- aux cont e1
                (e2t, e2') <- aux cont e2
                let res = ("Bool", Eq e1' e2')
                when (e1t /= e2t && (e1t `elem` ["Int", "Bool", "String"]
                                    || e2t `elem` ["Int", "Bool", "String"]))
                    $ Left "bad testing for equality"
                return res

            Leq e1 e2 -> comp cont Leq e1 e2

            Comp e -> do
                (et, e') <- aux cont e
                unless (et=="Int")
                    $ Left "~ argument is not integer"
                return ("Int", Comp e')

            IntConst s -> return ("Int", IntConst s)

            StringConst s -> return ("String", StringConst s)

            BoolConst b -> return ("Bool", BoolConst b)

            New t -> return (t, New t)

            IsVoid e -> do
                (_, e') <- aux cont e
                return ("Bool", IsVoid e')

            NoExpr -> return ("_no_type", NoExpr)

            Object s -> (\x -> (x, Object x)) <$> getType cont s

        aux:: Context -> UExpr -> Either String (String, TExpr)
        aux cont (Expr NT expr') = (\(t, e) -> (t, Expr t e)) <$> aux' cont expr'
        arith cont c e1 e2 = do
            (e1t, e1') <- aux cont e1
            (e2t, e2') <- aux cont e2
            unless (e1t == "Int" && e2t == "Int")
                $ Left "no integer arithmetics"
            return ("Int", c e1' e2')
        comp cont c e1 e2 =  do
            (e1t, e1') <- aux cont e1
            (e2t, e2') <- aux cont e2
            unless (e1t == "Int" && e2t == "Int")
                $ Left "no integer comparison"
            return ("Bool", c e1' e2')



type Context = (G.GlobalEnv, String, M.Map String String)

lift:: (G.GlobalEnv -> a) -> Context -> a
lift f (env, _, _) = f env

isSubtype :: Context -> String -> String -> Bool
isSubtype = lift G.isSubtype

superType :: Context -> [String] -> String
superType = lift G.superType

getMethod :: Context -> String -> String -> Either String [String]
getMethod = lift G.getMethod

currentClass :: Context -> String
currentClass (_, cls, _) = cls

getType :: Context -> String -> Either String String
getType  (_, _, objects) s = case M.lookup s objects of
    Just t -> pure t
    Nothing -> Left "unknown identifier"

addToContext :: Context -> String -> String -> Context
addToContext (env, c, objects) name type_ = (env, c, M.insert name type_ objects)
