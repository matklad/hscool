module Hscool.Semant.TypeEnv (getTypeEnv, join, isSubtype) where
import           Control.Applicative ((<$>))
import qualified Data.Map            as M

data TypeEnv = TypeEnv (M.Map String (String, [String]))
  deriving Show


getTypeEnv :: [(String, String)] -> Either String TypeEnv
getTypeEnv cs = TypeEnv <$> foldl addOne (Right $ M.fromList [(c, (s, [])) | (c, s) <- cs]) cs
  where
    addOne eitherM (cls, super) = do
      if super `elem` ["Int", "Bool", "String", "SELF_TYPE"]
        then Left $ "Can't inherit from " ++ super
        else return ()

      if super == cls
        then Left $ "Can't inherit from self: " ++ cls
        else return ()

      m <- eitherM
      return $ M.adjust (\(x, ls) -> (x, cls:ls)) super m

join:: TypeEnv -> [String] -> String
join env@(TypeEnv m) ts = let join' = join env
                              isSubtype' = isSubtype env
  in case ts of
    [] -> error "empty join"
    [t] -> error "one join"
    [a, b] -> case (isSubtype' a b, isSubtype' b a) of
      (True, False) -> b
      (False, True) -> a
      (False, False) -> let as = fst $ m M.! a in
        join' [as, b]
    (t:ts) -> join' [t, join' ts]

isSubtype:: TypeEnv -> String -> String -> Bool
isSubtype env@(TypeEnv m) t s = if t == s
  then True
  else foldl (||) True $ map (isSubtype env t) (snd $ m M.! s)





