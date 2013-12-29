module Syntax.Internal where

import Control.Applicative
import Data.Traversable
import Data.Typeable
import Data.Foldable

import qualified Syntax.Abs as Abs
import Utils

-- Pointers ---------------------------------------------------------------

newtype Ptr = Ptr Integer
    deriving (Eq, Ord)

instance Show Ptr where
    show (Ptr n) = "*" ++ show n

newtype TypeX a	 = TypePtr   Ptr deriving (Eq, Typeable)
newtype TermX a	 = TermPtr   Ptr deriving (Eq, Typeable)
newtype Clause	 = ClausePtr Ptr deriving (Eq, Typeable)
newtype Pair a b = PairPtr   Ptr deriving (Eq, Typeable)
newtype Unit     = UnitPtr   Ptr deriving (Eq, Typeable)

type Type = TypeX Term
type Term = TermX Name

class (Show ptr, Eq ptr, Show a, Typeable a) => Pointer ptr a | ptr -> a, a -> ptr where
    toRawPtr   :: ptr -> Ptr
    fromRawPtr :: Ptr -> ptr

instance Pointer Unit ()          where toRawPtr (UnitPtr p)   = p; fromRawPtr = UnitPtr
instance (Show a, Typeable a) => Pointer (TypeX a) (TypeX' a)	  where toRawPtr (TypePtr p)   = p; fromRawPtr = TypePtr
instance (Show a, Typeable a) => Pointer (TermX a) (TermX' a)	  where toRawPtr (TermPtr p)   = p; fromRawPtr = TermPtr
instance Pointer Clause Clause'   where toRawPtr (ClausePtr p) = p; fromRawPtr = ClausePtr
instance (Show a, Show b, Typeable a, Typeable b) =>
	 Pointer (Pair a b) (a,b) where
    toRawPtr (PairPtr p)   = p
    fromRawPtr = PairPtr

instance (Show a, Typeable a) => Show (TypeX a)	 where show = show . toRawPtr
instance (Show a, Typeable a) => Show (TermX a)	 where show = show . toRawPtr
instance Show Clause	 where show = show . toRawPtr
instance Show (Pair a b) where show (PairPtr p) = show p
instance Show Unit       where show = show . toRawPtr

-- Syntax -----------------------------------------------------------------

type Arity = Int

data Definition
	= Axiom Name Type
	| Defn  Name Type [Clause]
	| Data  Name Type [Constructor]
	| Cons  Name Type
    deriving (Show, Typeable)

data Clause' = Clause [Pattern] Term
    deriving (Show, Typeable)

data Constructor = Constr Name Arity
    deriving (Show, Typeable)

data Pattern = VarP Name
	     | ConP Name [Pattern]
    deriving (Show, Typeable)

type Name	   = String
type DeBruijnIndex = Integer

type TelescopeX a = [RBind (TypeX a)]
type Telescope = TelescopeX Term
data RBind a = RBind String a
  deriving (Show)

data TypeX' a = Pi (TypeX a) (Abs TypeX a)
           | RPi (TelescopeX a) (TypeX a)
	   | Fun (TypeX a) (TypeX a)
	   | El a
	   | Set
    deriving (Show, Typeable)

type Type' = TypeX' Term
    
data TermX' a = Lam (Abs TermX a)
	   | App (TermX a) (TermX a)
	   | Var DeBruijnIndex
	   | Def a
    deriving (Show, Typeable)

type Term' = TermX' Name

data Abs f a = Abs { absName :: Name, absBody :: f a }
    deriving (Functor, Foldable, Traversable)

instance Show (f a) => Show (Abs f a) where
    show (Abs _ b) = show b

