module Hscool.Semant.TypeEnv (getTypeEnv, getClass, superType, isSubtype, TypeEnv, object, checkDefined) where
import           Control.Applicative ((<$>), pure)
import           Control.Monad       (when)
import           Data.List           (nub, (\\))
import qualified Data.Map            as M
import           Hscool.Types.AST

data TypeEnv = TypeEnv (M.Map String (String, [String], UClass))
  deriving Show


getTypeEnv :: [UClass] -> Either String TypeEnv
getTypeEnv cs = do
        cs' <- simpleCheck cs
        TypeEnv <$> foldl
            addOne
            (Right $ M.fromList [(name, (super, [], c)) | c@(Class name super _ _) <- cs'])
            cs'
    where
        addOne eitherM (Class name super _ _) = do
            when (super `elem` ["Int", "Bool", "String", "SELF_TYPE"])
                 $ Left ("Can't inherit from " ++ super)
            when (super == name)
                 $ Left $ "Can't inherit from self: " ++ name
            m <- eitherM
            return $ M.adjust (\(x, ls, c) -> (x, name:ls, c)) super m

superType :: TypeEnv -> [String] -> String
superType env@(TypeEnv m) ts = let
    superType' = superType env
    isSubtype' = isSubtype env
    in case ts of
        [] -> error "empty superType"
        [_] -> error "one superType"
        [a, b] -> case (isSubtype' a b, isSubtype' b a) of
            (True, False) -> b
            (False, True) -> a
            (False, False) -> let (as, _, _) =  m M.! a in
                superType' [as, b]
        (t:ts') -> superType' [t, superType' ts']

isSubtype :: TypeEnv -> String -> String -> Bool
isSubtype env@(TypeEnv m) t s = (t == "_no_type") || (t == s) ||
    foldl (||) True (map (isSubtype env t) (let (_,ls,_) = m M.! s in ls))

getClass :: TypeEnv -> String -> UClass
getClass (TypeEnv m) name = let (_,_,c) = m M.! name in c

checkDefined :: TypeEnv -> String -> Either String ()
checkDefined (TypeEnv m) c = case M.lookup c m of
    Nothing -> Left $ "class is not defined: " ++ c
    Just _ -> pure()

simpleCheck :: [UClass] -> Either String [UClass]
simpleCheck cls =
    case cls' \\ nub cls' of
        [] -> Right cls'
        cs -> Left $ "Class redeclaration: " ++ show [name | (Class name _ _ _)<- cs]
    where
        cls' = buildin ++ cls

ne :: UExpr
ne = Expr NT NoExpr

object :: UClass
object = Class "Object" "_"
    [ Method "abort" [] "Object" ne
    , Method "type_name" [] "String" ne
    , Method "copy" [] "SELF_TYPE" ne]
    "_"

io :: UClass
io = Class "IO" "Object"
    [ Method "out_string" [Formal "x" "String"] "SELF_TYPE" ne
    , Method "out_int" [Formal "x" "Int"] "SELF_TYPE" ne
    , Method "in_string" [] "String" ne
    , Method "in_int" [] "Int" ne ]
    "_"

int :: UClass
int = Class "Int" "Object" [] "_"

string :: UClass
string = Class "String" "Object"
    [ Method "length" [] "Int" ne
    , Method "concat" [Formal "s" "String"] "String" ne
    , Method "substr" [Formal "i" "Int", Formal "l" "Int"] "String" ne]
    "_"

bool :: UClass
bool = Class "Bool" "Object" [] "_"

buildin :: [UClass]
buildin = [object, io, int, string, bool]
