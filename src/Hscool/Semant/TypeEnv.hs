module Hscool.Semant.TypeEnv (getTypeEnv, getClass, join, isSubtype, TypeEnv, object) where
import           Control.Applicative ((<$>))
import           Control.Monad       (when)
import           Data.List           (nub, (\\))
import qualified Data.Map            as M
import           Hscool.Types.AST

data TypeEnv = TypeEnv (M.Map String (String, [String], UClass))
  deriving Show


getTypeEnv :: [UClass] -> Either String TypeEnv
getTypeEnv cs = do
  cs' <- simpleCheck cs
  TypeEnv <$> foldl addOne
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

join :: TypeEnv -> [String] -> String
join env@(TypeEnv m) ts = let join' = join env
                              isSubtype' = isSubtype env
  in case ts of
    [] -> error "empty join"
    [_] -> error "one join"
    [a, b] -> case (isSubtype' a b, isSubtype' b a) of
      (True, False) -> b
      (False, True) -> a
      (False, False) -> let (as, _, _) =  m M.! a in
        join' [as, b]
    (t:ts') -> join' [t, join' ts']

isSubtype :: TypeEnv -> String -> String -> Bool
isSubtype env@(TypeEnv m) t s = (t == s) ||
  foldl (||) True (map (isSubtype env t) (let (_,ls,_) = m M.! s in ls))

getClass :: TypeEnv -> String -> UClass
getClass (TypeEnv m) name = let (_,_,c) = m M.! name in c


simpleCheck :: [UClass] -> Either String [UClass]
simpleCheck cls =
  case cls' \\ nub cls' of
    [] -> Right cls'
    cs -> Left $ "Class redeclaration: " ++ show [name | (Class name _ _ _)<- cs]
  where
    cls' = buildin ++ cls

object :: UClass
object = Class "Object" "_" [] "_"
io :: UClass
io = Class "IO" "Object" [] "_"
int :: UClass
int = Class "Int" "_" [] "_"
string :: UClass
string = Class "String" "_" [] "_"
bool :: UClass
bool = Class "Bool" "_" [] "_"

buildin :: [UClass]
buildin = [object, io, int, string, bool]
