module Hscool.Semant.TypeCheck where
import           Hscool.Semant.TypeEnv
import           Hscool.Types.AST
import           Control.Applicative ((<$>), (<*>))
import qualified Control.Monad as Monad
import qualified Data.Map as M
import Data.List(find)

typeCheck :: TypeEnv -> UProgram -> Either String TProgram
typeCheck env (Program classes)= Program <$>  mapM (typeCheckClass env) classes

typeCheckClass :: TypeEnv -> UClass -> Either String TClass
typeCheckClass env c@(Class name type_ features file) =
        Class name type_ 
        <$> Monad.join (mapM <$> (typeCheckFeature env <$> attr <*> methods) <*> return features)
        <*> return file
    where
        attr = getAttrs env c
        methods = getMethods env c

type TypeMap  t = M.Map String t
getAttrs :: TypeEnv -> UClass -> Either String (TypeMap String)
getAttrs env c@(Class _ super _ _) = if c == object
        then return $ attrs object
        else Monad.join $ joinAttrs <$> superAttrs <*> return myAttrs
    where
        attrs (Class _ _ features _) = M.fromList 
            [(name, type_) | (Attribute name type_ _) <- features]
        s = getClass env super
        myAttrs = attrs c
        superAttrs = getAttrs env s
        joinAttrs m1 m2 = let m = M.intersection m1 m2 in
            if M.null m 
            then return $ M.union m1 m2
            else Left $ "Attribute redefinition " ++ show (M.keys m)

getMethods :: TypeEnv -> UClass -> Either String (TypeMap [String])
getMethods env c@(Class _ super _ _) = if c == object
        then return $ methods object
        else Monad.join $ joinMethods <$> superMethods <*> return myMethods
    where
        methods (Class _ _ features _) = M.fromList 
            [(name, type_: types formals) | (Method name formals type_ _) <- features]
        types = map (\(Formal _ type_) -> type_)
        s = getClass env super
        myMethods = methods c
        superMethods = getMethods env s
        joinMethods m1 m2 = let overriden = M.keys $ M.intersection m1 m2 in
            case find (\k -> m1 M.! k /= m2 M.! k) overriden of
                Nothing -> return $ M.union m1 m2
                Just x -> Left $ "wrong method signature in subclass: " ++ x

typeCheckFeature :: TypeEnv -> TypeMap String -> TypeMap [String] -> UFeature -> Either String TFeature
typeCheckFeature env attrs methods feature = case feature of
        Method name formals type_ expr -> let expr'@(Expr t _) = typeCheckExpr env expr in
            if isSubtype' t type_ 
            then return $ Method name formals type_ expr'
            else Left $ "Method body has wrong return type: " ++ name
        Attribute name type_ expr -> let expr'@(Expr t _) = typeCheckExpr env expr in  
            if isSubtype' t type_ 
            then return $ Attribute name type_ expr'
            else Left $ "Attribute initialization has wrong type: " ++ name
    where
        isSubtype' = isSubtype env

typeCheckExpr :: TypeEnv -> UExpr -> TExpr
typeCheckExpr = undefined