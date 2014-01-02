{-# LANGUAGE ImpredicativeTypes #-}
module TypeChecker.Monad.Context where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Bound

import Syntax.Internal
import TypeChecker.Monad
import TypeChecker.Monad.Heap
import TypeChecker.DeBruijn
import Utils

getContext :: TC Context
getContext = asks envContext

withContext :: (Context -> Context) -> TC a -> TC a
withContext f = local $ \e -> e { envContext = f (envContext e) }

extendContext :: Name -> Scope Int TypeX Term -> TC a -> TC a
extendContext x t = withContext (VBind x t :)

extendContext_ :: Name -> TC a -> TC a
extendContext_ x m = do
  set <- evaluated Set
  extendContext x (Scope set) m

extendContextTel :: TelescopeX (Scope Int TypeX b) -> TC a -> TC a
extendContextTel tel = withContext (TBind (reverse tel) :)

(!) :: Context -> Name -> Maybe (DeBruijnIndex, DeBruijnIndex, Scope Int TypeX (forall a. a))
ctx ! x = look 0 ctx
    where
	look n (VBind y t : ctx)
	    | x == y    = return (n, n + 1, fmap undefined t)
	    | otherwise = look (n + 1) ctx
        look n (TBind tel : ctx) =
          lookTel n n tel `mplus` look n' ctx
          where n' = n + genericLength tel
		lookTel n m (RBind y t : tel)
		  | x == y    = return (n, m, fmap undefined t)
		  | otherwise = lookTel (n + 1) m tel
		lookTel _ _ [] = fail ""
	look _ [] = fail ""


lookupContext :: Name -> TC (DeBruijnIndex, Scope Int TypeX (forall a. a))
lookupContext x = do
    ctx <- getContext
    case ctx ! x of
	Just (n, m, t) -> (,) n <$> raiseBy m t
	Nothing	       -> fail $ "Unbound variable: " ++ x

flattenContext :: Context -> [(Name, Scope Int TypeX (forall a. a))]
flattenContext = concatMap f
  where
    f (VBind x t) = [(x, t)]
    f (TBind tel) = [ (x, t) | RBind x t <- tel ]

getVarName :: DeBruijnIndex -> TC String
getVarName n = do
    ctx <- getContext
    fst <$> (ctx ! n)
    where
	cxt ! n
	    | len <= n  = fail $ "deBruijn index out of range " ++ show n ++ " in " ++ show xs
	    | otherwise = return $ xs !! fromIntegral n
	    where
		len = fromIntegral $ length xs
                xs  = flattenContext cxt

