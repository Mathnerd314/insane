
module TypeChecker.DeBruijn where

import Control.Applicative
import Data.Traversable hiding (mapM)
import Data.Typeable
import Bound

import Syntax.Internal
import TypeChecker.Monad
import TypeChecker.Monad.Heap
import Utils

class DeBruijn a where
    transform :: (Integer -> DeBruijnIndex -> TC Term') -> Integer -> a -> TC a

transformX :: (Pointer ptr a, DeBruijn a) => (Integer -> DeBruijnIndex -> TC Term') -> Integer -> ptr -> TC ptr
transformX f n = liftPtrM (transform f n)

instance (DeBruijn a, Typeable a, Show a) => DeBruijn (TypeX a) where transform = transformX
instance DeBruijn Term where transform = transformX

instance (DeBruijn a, Typeable a, Show a) => DeBruijn (TypeX' a) where
    transform f n t = case t of
	Pi a b	-> uncurry Pi  <$> trf (a,b)
        RPi tel a -> uncurry RPi <$> transform f n' (tel, a)
          where n' = n + fromIntegral (length tel)
	Fun a b -> uncurry Fun <$> trf (a,b)
	El t	-> El <$> trf t
	Set	-> return Set
	where
	    trf :: (DeBruijn a) => a -> TC a
	    trf = transform f n

instance DeBruijn a => DeBruijn (RBind a) where
    transform f n (RBind x a) =
      RBind x <$> transform f n a

instance DeBruijn Char where
   transform _ _ = return
      
instance DeBruijn Term' where
    transform f n t = case t of
	Def f	-> Def <$> trf f
	Var m	-> f n m
	App s t	-> uncurry App <$> trf (s,t)
	Lam t	-> Lam <$> trf t
	where
	    trf :: (DeBruijn a) => a -> TC a
	    trf = transform f n

instance (DeBruijn a, DeBruijn b) => DeBruijn (a,b) where
    transform f n (x,y) = (,) <$> transform f n x <*> transform f n y

instance DeBruijn a => DeBruijn (Abs a) where
    transform f n (Abs x b) = Abs x <$> transform f (n + 1) b

instance DeBruijn a => DeBruijn [a] where
    transform f n = traverse (transform f n)

raiseByFrom :: DeBruijn a => Integer -> Integer  -> a -> TC a
raiseByFrom k = transform f
    where
	f :: Integer -> DeBruijnIndex -> TC (TermX' b)
	f n m | m < n	  = return $ Var m
	      | otherwise = return $ Var (m + k)

raiseBy :: DeBruijn a => Integer -> a -> TC a
raiseBy k = raiseByFrom k 0

raise :: DeBruijn a => a -> TC a
raise = raiseBy 1

substUnder  :: DeBruijn a => Integer -> Term -> a -> TC a
substUnder n0 t = transform f n0
    where
	f n m | m < n	  = return $ Var m
	      | m == n	  = forceClosure =<< raiseByFrom (n - n0) n0 t
	      | otherwise = return $ Var (m - 1)

subst :: DeBruijn a => Term -> Abs a -> TC a
subst t = substUnder 0 t . absBody

substs :: DeBruijn a => [Term] -> a -> TC a
substs []     a = return a
substs (t:ts) a = substUnder 0 t =<< flip substs a =<< raise ts


{-
class MonadLike f where
  bind :: (Typeable a, Show a, Typeable b, Show b) => (a -> TC (f b)) -> f a -> TC (f b)
  ret :: a -> TC (f a)

instance MonadLike TypeX' where
  ret = return . El
  bind f x = case x of
	Pi a (Abs n b) -> Pi  <$> liftPtrM (bind f) a <*> (Abs n <$> magic f b)
        RPi tel a -> RPi <$> mapM (\(RBind n k) -> RBind n <$> (liftPtrM (bind f) k)) tel <*> liftPtrM (bind f) a
	Fun a b -> Fun <$> liftPtrM (bind f) a <*> liftPtrM (bind f) b
	El t	-> f t
	Set	-> return Set

magic :: (Show k, Typeable k, Show a, Typeable a, Show b, Typeable b,
	      Typeable (g a),
	      Typeable (g b),
	      Pointer (g (Var k (g a))) (f (Var k (g a))),
	      Pointer (g (Var k (g b))) (f (Var k (g b))),
	      Pointer (g a) (f a),
	      Pointer (g b) (f b),
	      MonadLike f) =>
	       (a -> TC (f b)) -> Scope k g a -> TC (Scope k g b)
magic f (Scope s) = Scope <$> liftPtrM (bind (\x -> ret =<< (m3 f x))) s

m3 = traverse . liftPtrM . bind

-}