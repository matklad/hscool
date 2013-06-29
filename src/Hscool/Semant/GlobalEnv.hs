module Hscool.Semant.GlobalEnv (GlobalEnv, getGlobalEnv, isSubtype, superType, getAttrs, getMethod, assureMain) where
import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Monad         (join)
import           Data.List             (find)
import qualified Data.Map              as M
import qualified Hscool.Semant.TypeEnv as T
import           Hscool.Types.AST


type AttrEnv = M.Map String (M.Map String String)
type MethodEnv = M.Map String (M.Map String [String])
type GlobalEnv = (T.TypeEnv, AttrEnv, MethodEnv)

getGlobalEnv :: T.TypeEnv -> Either String GlobalEnv
getGlobalEnv tenv = let
        classes = T.extractClasses tenv
        getAttributes :: UClass -> Either String (M.Map String String)
        getAttributes c@(Class _ super _ _) = if c == T.object
                then return $ attrs T.object
                else join $ joinAttrs <$> superAttrs <*> return myAttrs
            where
                attrs (Class _ _ features _) = M.fromList
                    [(name, type_) | (Attribute name type_ _) <- features]
                s = T.getClass tenv super
                myAttrs = attrs c
                superAttrs = getAttributes s
                joinAttrs m1 m2 = let m = M.intersection m1 m2 in
                    if M.null m
                    then return $ M.union m1 m2
                    else Left $ "Attribute redefinition " ++ show (M.keys m)

        getMethods :: UClass -> Either String (M.Map String [String])
        getMethods c@(Class _ super _ _) = if c == T.object
                then return $ methods T.object
                else join $ joinMethods <$> superMethods <*> pure myMethods
            where
                methods (Class _ _ features _) = M.fromList
                    [(name, type_: types formals) | (Method name formals type_ _) <- features]
                types = map (\(Formal _ type_) -> type_)
                s = T.getClass tenv super
                myMethods = methods c
                superMethods = getMethods s
                joinMethods m1 m2 = let overriden = M.keys $ M.intersection m1 m2 in
                    case find (\k -> m1 M.! k /= m2 M.! k) overriden of
                        Nothing -> return $ M.union m1 m2
                        Just x  -> Left $ "wrong method signature in subclass: " ++ x

        classNames = [name | (Class name _ _ _) <- classes]
        aux f = M.fromList <$> (zip classNames <$> mapM f classes)
    in do
        attrm <- aux getAttributes
        methm <- aux getMethods
        return (tenv, attrm, methm)

isSubtype :: GlobalEnv -> String -> String -> Bool
isSubtype (tenv, _, _) = T.isSubtype tenv

superType :: GlobalEnv -> [String] -> String
superType (tenv, _, _) = T.superType tenv

getAttrs :: GlobalEnv -> String -> M.Map String String
getAttrs (_, attrm, _) c = attrm M.! c

getMethod :: GlobalEnv -> String -> String -> Either String [String]
getMethod (_, _, methm) c m = case M.lookup m (methm M.! c) of
    Just x -> pure x
    Nothing -> Left $ "Class " ++ c ++ " doesn't define method " ++ m

assureMain :: GlobalEnv -> Either String ()
assureMain env@(tenv, _, _) = do
    T.checkDefined tenv "Main"
    getMethod env "Main" "main"
    return ()
