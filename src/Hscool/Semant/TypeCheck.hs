module Hscool.Semant.TypeCheck where
import           Control.Applicative   ((<$>), (<*>), pure)
import qualified Control.Monad         as Monad
import           Data.List             (find, nub)
import qualified Data.Map              as M
import           Hscool.Semant.TypeEnv
import           Hscool.Types.AST


typeCheck :: TypeEnv -> UProgram -> Either String TProgram
typeCheck tenv (Program classes)= do
    env <- getStaticEnv tenv classes
    Program <$>  mapM (typeCheckClass env) classes

typeCheckClass :: StaticEnv -> UClass -> Either String TClass
typeCheckClass env c@(Class name type_ features file) =
        Class name type_
        <$> mapM (typeCheckFeature env c) features
        <*> return file


typeCheckFeature :: StaticEnv -> UClass -> UFeature -> Either String TFeature
typeCheckFeature (tenv, attrm, methm) (Class cname _ _ _) feature = case feature of
        Method name formals type_ expr -> do
            objects <- addFormals (attrm M.! cname) formals
            let context = (tenv, objects, methm)
            let expr'@(Expr t _) = typeCheckExpr context expr
            if isSubtype' t type_
                then return $ Method name formals type_ expr'
                else Left $ "Method body has wrong return type: " ++ name

        Attribute name type_ expr ->
            let
                context = (tenv, attrm M.! cname, methm)
                expr'@(Expr t _) = typeCheckExpr context expr
            in
                if isSubtype' t type_
                then return $ Attribute name type_ expr'
                else Left $ "Attribute initialization has wrong type: " ++ name
    where
        isSubtype' = isSubtype tenv
        addFormals objects formals = if formals == nub formals
            then pure $ M.union
                        (M.fromList [(name, type_)| (Formal name type_) <- formals])
                        objects
            else Left "formal parameters with duplicate names!"

typeCheckExpr :: Context -> UExpr -> TExpr
typeCheckExpr = undefined


type AttrEnv = M.Map String (M.Map String String)
type MethodEnv = M.Map String (M.Map String [String])
type StaticEnv = (TypeEnv, AttrEnv, MethodEnv)

type Context = (TypeEnv, M.Map String String, MethodEnv)

getStaticEnv :: TypeEnv -> [UClass] -> Either String StaticEnv
getStaticEnv tenv classes = let
        getAttrs :: UClass -> Either String (M.Map String String)
        getAttrs c@(Class _ super _ _) = if c == object
                then return $ attrs object
                else Monad.join $ joinAttrs <$> superAttrs <*> return myAttrs
            where
                attrs (Class _ _ features _) = M.fromList
                    [(name, type_) | (Attribute name type_ _) <- features]
                s = getClass tenv super
                myAttrs = attrs c
                superAttrs = getAttrs s
                joinAttrs m1 m2 = let m = M.intersection m1 m2 in
                    if M.null m
                    then return $ M.union m1 m2
                    else Left $ "Attribute redefinition " ++ show (M.keys m)

        getMethods :: UClass -> Either String (M.Map String [String])
        getMethods c@(Class _ super _ _) = if c == object
                then return $ methods object
                else Monad.join $ joinMethods <$> superMethods <*> return myMethods
            where
                methods (Class _ _ features _) = M.fromList
                    [(name, type_: types formals) | (Method name formals type_ _) <- features]
                types = map (\(Formal _ type_) -> type_)
                s = getClass tenv super
                myMethods = methods c
                superMethods = getMethods s
                joinMethods m1 m2 = let overriden = M.keys $ M.intersection m1 m2 in
                    case find (\k -> m1 M.! k /= m2 M.! k) overriden of
                        Nothing -> return $ M.union m1 m2
                        Just x  -> Left $ "wrong method signature in subclass: " ++ x

        classNames = [name | (Class name _ _ _) <- classes]
        aux f = M.fromList <$> (zip classNames <$> mapM f classes)
    in do
        attrm <- aux getAttrs
        methm <- aux getMethods
        return (tenv, attrm, methm)
