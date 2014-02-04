
module TypeChecker.Reduce where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Data.Monoid
import Data.Foldable hiding (foldr1)
import Data.Traversable hiding (mapM)
import Data.Typeable
import Bound

import Syntax.Internal
import TypeChecker.Monad
import TypeChecker.Monad.Heap
import TypeChecker.Monad.Signature
import TypeChecker.Monad.Context
import TypeChecker.DeBruijn
import TypeChecker.Print
import TypeChecker.Force
import Utils

type RedexView = RedexViewX (Var DeBruijnIndex (TermX' Name))

data RedexViewX a
        = Iota Name       [TermX a] -- | The number of arguments should be the same as the arity.
        | Beta (Abs (TermX a)) (TermX a)
        | NonRedex (TermX' a)
  deriving (Show)

type ConView = ConViewX (Var DeBruijnIndex (TermX' Name))
data ConViewX a = ConApp Name [TermX a]
             | NonCon (TermX' a)

class SpineX a where
  spineX :: (Typeable b, Show b) => TermX' a -> (TermX' a -> TermX' b) -> TC [(TermX b, TermX' b)]
  redexViewX :: (Typeable b, Show b) => TermX' a -> (TermX' a -> TermX' b) -> TC (RedexViewX b)
  conViewX :: (Typeable b, Show b) => TermX' a -> (TermX' a -> TermX' b) -> TC (ConViewX b)

instance SpineX Name where
  spineX p f = 
    case p of
        App s' t -> do
            s <- forceClosure s'
            sp <- spineX s f
            t' <- liftPtr f t
            return $ sp ++ [(t', f p)]
        _  -> do
          p' <- evaluated (f p)
          return [(p',f p)]
  redexViewX p f = do
      (h,_) : args <- spineX p id
      t <- forceClosure h
      case t of
          Def c -> do
            ar <- functionArity c
            case ar of
                Just n | n == length args
                  -> Iota c <$> mapM (liftPtr f . fst) args
                _ -> other
          Lam s -> case args of
              [(t,_)] -> Beta <$> traverse (liftPtr f) s <*> liftPtr f t
              _     -> other
          App s t       -> fail "redexView: impossible App"
    where
        other = return $ NonRedex (f p)
  conViewX t f = do
      (c,_):args <- spineX t id
      s <- forceClosure c
      case s of
          Def c -> ConApp c <$> mapM (liftPtr f . fst) args
          _       -> return $ NonCon (f t)

instance (SpineX a, Typeable a, Typeable b, Show a, Show b) => SpineX (Var b (TermX' a)) where
  spineX p f =
    case p of
        Def (F b) -> spineX b (f . Def . F)
        App s' t -> do
            s <- forceClosure s'
            sp <- spineX s f
            t' <- liftPtr f t
            return $ sp ++ [(t', f p)]
        _  -> do
          p' <- evaluated (f p)
          return [(p',f p)]
  redexViewX p f = do
    (h,_):args <- spineX p id
    t <- forceClosure h
    case t of
        Def (B x) -> other
        Def (F c) -> redexViewX c (f . Def . F)
        Lam s -> case args of
            [(t,_)] -> Beta <$> traverse (liftPtr f) s <*> liftPtr f t
            _     -> other
        App s t -> fail "redexView: impossible App"
    where
      other = return $ NonRedex (f p)
  conViewX t f = do
      (c,_):args <- spineX t id
      s <- forceClosure c
      case s of
          Def (F c) -> conViewX c (f . Def. F)
          _ -> return $ NonCon (f t)


-- | @spine (f a b c) = [(f, f), (a, f a), (b, f a b), (c, f a b c)]@
spine :: (Show a, Typeable a, SpineX a) => TermX a -> TC [(TermX a, TermX a)]
spine t = do
  p <- forceClosure t
  xs <- spineX p id
  mapM (\(x,y) -> (,) x <$> evaluated y) xs

redexView :: (Show a, Typeable a, SpineX a) => TermX a -> TC (RedexViewX a)
redexView p = do
  t <- forceClosure p
  redexViewX t id

conView :: (Show a, Typeable a, SpineX a) => TermX a -> TC (ConViewX a)
conView p = do
    t <- forceClosure p
    conViewX t id

data Progress = NoProgress | YesProgress

instance Monoid Progress where
    mempty                          = NoProgress
    mappend NoProgress p            = p
    mappend p NoProgress            = p
    mappend YesProgress YesProgress = YesProgress

whenProgress :: Monad m => Progress -> m a -> m ()
whenProgress YesProgress m = m >> return ()
whenProgress NoProgress  m = return ()

data Match a = YesMatch a | MaybeMatch | NoMatch deriving (Functor)

instance Monoid a => Monoid (Match a) where
    mempty               = YesMatch mempty
    mappend NoMatch _    = NoMatch
    mappend _ NoMatch    = NoMatch
    mappend MaybeMatch _ = MaybeMatch
    mappend _ MaybeMatch = MaybeMatch
    mappend (YesMatch ts) (YesMatch ss) = YesMatch $ ts `mappend` ss

instance Foldable Match where
    foldMap f (YesMatch x) = f x
    foldMap f NoMatch      = mempty
    foldMap f MaybeMatch   = mempty

instance Traversable Match where
    traverse f (YesMatch x) = YesMatch <$> f x
    traverse f NoMatch      = pure NoMatch
    traverse f MaybeMatch   = pure MaybeMatch

choice :: Match a -> Match a -> Match a
choice NoMatch m = m
choice m       _ = m

class WHNF a where
    whnf :: a -> TC Progress

instance (WHNF a, WHNF b) => WHNF (a,b) where
    whnf (x,y) = mappend <$> whnf x <*> whnf y

whnfX :: (Pointer ptr a, WHNF a) => ptr -> TC Progress
whnfX p = do
      t <- forceClosure p
      whnf t

instance (WHNF a, Typeable a, Show a) => WHNF (TypeX a) where whnf = whnfX
instance (WHNF a, Typeable a, Show a) => WHNF (TypeX' a) where
    whnf a =
      case a of
            RPi _ _ -> return NoProgress
            Pi _ _  -> return NoProgress
            Fun _ _ -> return NoProgress
            Set     -> return NoProgress
            El t    -> whnf t

instance WHNF (TermX Name) where
    whnf p = do
        p' <- forceClosure p :: TC (TermX' Name)
        v <- redexViewX p' id
        case v of
            NonRedex t -> case t of
                App s t -> do
                    pr <- whnf s
                    whenProgress pr $ whnf p
                    return pr
                Lam _   -> return NoProgress
                Def _ -> return NoProgress
            Beta s t    -> do
                poke p (forceClosure =<< subst t s)
                whnf p
            Iota f ts   -> do
                Defn _ _ cs <- getDefinition f
                m           <- match cs ts
                case m of
                    YesMatch t -> do
                        poke p (forceClosure t)
                        whnf p
                    MaybeMatch -> return NoProgress
                    NoMatch    -> fail "Incomplete pattern matching"

instance WHNF (TermX (Var DeBruijnIndex (TermX' Name))) where
    whnf p = do
        p' <- forceClosure p
        v <- redexViewX p' id
        case v of
            NonRedex t -> case t of
                App s t -> do
                    pr <- whnf s
                    whenProgress pr $ whnf p
                    return pr
                Lam _   -> return NoProgress
                Def (B _) -> return NoProgress
                Def (F _) -> return NoProgress
            Beta s t    -> do
                poke p (forceClosure =<< subst t s)
                whnf p
            Iota f ts   -> do
                Defn _ _ cs <- getDefinition f
                m           <- match cs ts
                case m of
                    YesMatch t -> do
                        poke p (forceClosure t)
                        whnf p
                    MaybeMatch -> return NoProgress
                    NoMatch    -> fail "Incomplete pattern matching"

type MatchConstraint a = (Show a, Typeable a, SpineX a, WHNF (TermX a))
-- | Invariant: there are the same number of terms as there are patterns in the clauses
match :: MatchConstraint a => [ClauseX (TermX a)] -> [TermX a] -> TC (Match (TermX a))
match cs ts = foldr1 choice <$> mapM (flip matchClause ts) cs

matchClause :: MatchConstraint a => ClauseX (TermX a) -> [TermX a] -> TC (Match (TermX a))
matchClause c ts = do
    Clause ps t <- forceClosure c
    m <- matchPatterns ps ts
    traverse (\ss -> substs ss t) m

matchPatterns :: MatchConstraint a => [Pattern] -> [TermX a] -> TC (Match [TermX a])
matchPatterns ps ts = mconcat <$> zipWithM matchPattern ps ts

matchPattern :: MatchConstraint a => Pattern -> TermX a -> TC (Match [TermX a])
matchPattern (VarP _) t = return $ YesMatch [t]
matchPattern (ConP c ps) t = do
    whnf t
    v <- conView t
    case v of
        ConApp c' ts
            | c == c'   -> matchPatterns ps (dropPars ts)
            | otherwise -> return NoMatch
        _               -> return MaybeMatch
  where
    dropPars ts = drop (length ts - length ps) ts
