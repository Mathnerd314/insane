
module Syntax.Internal where

import Control.Applicative
import Data.Traversable
import Data.Typeable
import Data.Foldable
import Prelude.Extras
import Bound

import qualified Syntax.Abs as Abs
import Utils

-- Pointers ---------------------------------------------------------------

newtype Ptr = Ptr Integer
    deriving (Eq, Ord)

instance Show Ptr where
    show (Ptr n) = "*" ++ show n

newtype TypeX a  = TypePtr   Ptr deriving (Eq, Typeable, Functor, Foldable, Traversable)
newtype TermX a  = TermPtr   Ptr deriving (Eq, Typeable, Functor, Foldable, Traversable)
newtype Clause   = ClausePtr Ptr deriving (Eq, Typeable)
newtype Pair a b = PairPtr   Ptr deriving (Eq, Typeable)
newtype Unit     = UnitPtr   Ptr deriving (Eq, Typeable)

instance Eq1 TypeX
instance Eq1 TermX

type Type = TypeX Term
type Term = TermX Name

type Pointer ptr a = (Show a, Typeable a, PointerX ptr a)

class PointerX ptr a | ptr -> a, a -> ptr where
    toRawPtr   :: ptr -> Ptr
    fromRawPtr :: Ptr -> ptr

instance PointerX Unit ()            where toRawPtr (UnitPtr p)   = p; fromRawPtr = UnitPtr
instance PointerX (TypeX a) (TypeX' a) where toRawPtr (TypePtr p) = p; fromRawPtr = TypePtr
instance PointerX (TermX a) (TermX' a) where toRawPtr (TermPtr p) = p; fromRawPtr = TermPtr
instance PointerX Clause Clause'     where toRawPtr (ClausePtr p) = p; fromRawPtr = ClausePtr
instance PointerX (Pair a b) (a,b)   where toRawPtr (PairPtr p)   = p; fromRawPtr = PairPtr

instance Show (TypeX a)  where show = show . toRawPtr
instance Show (TermX a)  where show = show . toRawPtr
instance Show Clause     where show = show . toRawPtr
instance Show (Pair a b) where show = show . toRawPtr
instance Show Unit       where show = show . toRawPtr
instance Show1 TermX
instance Show1 TypeX

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

type Name          = String
type DeBruijnIndex = Integer

type TelescopeX a = [RBind a]
type Telescope = TelescopeX (TypeX Term)
data RBind a = RBind String a
  deriving (Show, Functor, Foldable, Traversable)

data TypeX' a = Pi (TypeX a) (Abs (TypeX a))
           | RPi (TelescopeX (TypeX a)) (TypeX a)
           | Fun (TypeX a) (TypeX a)
           | El a
           | Set
    deriving (Show, Typeable, Functor)

type Type' = TypeX' Term
    
data TermX' a = Lam (Abs (TermX a))
           | App (TermX a) (TermX a)
           | Var DeBruijnIndex
           | Def a
    deriving (Show, Typeable, Functor)

type Term' = TermX' Name

data Abs a = Abs { absName :: Name, absBody :: a }
    deriving (Typeable, Functor, Foldable, Traversable)

instance Show a => Show (Abs a) where
    show (Abs _ b) = show b

