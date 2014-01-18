{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker.Monad where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Writer
import Data.Map as Map
import Data.Dynamic
import Bound

import Syntax.Internal

---------------------------------------------------------------------------
-- * Environment
---------------------------------------------------------------------------

data TCEnv   = TCEnv
    { envContext :: Context
    , envForce   :: HowMuchForce
    }

data HowMuchForce = Soft | Hard

emptyEnv :: TCEnv
emptyEnv = TCEnv
    { envContext = []
    , envForce   = Hard
    }

gently :: TC a -> TC a
gently = local $ \env -> env { envForce = Soft }

howForceful :: TC HowMuchForce
howForceful = asks envForce

---------------------------------------------------------------------------
-- ** Context
---------------------------------------------------------------------------

type Context = [ContextEntry]

data ContextEntry where
  VBind :: Name -> TypeX Term -> ContextEntry
  TBind :: TelescopeX (TypeX Term) -> ContextEntry

---------------------------------------------------------------------------
-- * State
---------------------------------------------------------------------------

data TCState = TCState
        { stHeap        :: Heap
        , stSig         :: Signature
        , stConstraints :: [Constraint]
        , stNextFree    :: Ptr
        }

initState :: TCState
initState = TCState
    { stHeap        = Map.empty
    , stSig         = Map.empty
    , stConstraints = []
    , stNextFree    = Ptr 0
    }

---------------------------------------------------------------------------
-- ** Signature
---------------------------------------------------------------------------

type Signature = Map Name Definition

type Constraint = Unit

getConstraints :: TC [Constraint]
getConstraints = gets stConstraints

---------------------------------------------------------------------------
-- ** Heap
---------------------------------------------------------------------------

type Heap = Map Ptr HeapObject

data HeapObject = forall a. (Show a, Typeable a) => HpObj (Closure a)

instance Show HeapObject where
    show (HpObj x) = show x

data Closure a = Evaluated a
               | Unevaluated (TCClosure a)

instance Show a => Show (Closure a) where
    show (Evaluated x)   = show x
    show (Unevaluated _) = "_"

data TCClosure a = TCCl
    { clEnv    :: TCEnv
    , clActive :: Active
    , clAction :: TC a
    }

data Active = Active | Inactive

setActive :: Active -> Closure a -> Closure a
setActive _ cl@Evaluated{} = cl
setActive a (Unevaluated cl) = Unevaluated cl{ clActive = a }

getActive :: Closure a -> Active
getActive Evaluated{} = Inactive
getActive (Unevaluated cl) = clActive cl

runTCClosure :: TCClosure a -> TC a
runTCClosure cl = local (const $ clEnv cl) $ clAction cl

---------------------------------------------------------------------------
-- * Error
---------------------------------------------------------------------------

data TCError = Fail String

instance Show TCError where
    show (Fail s) = "fail: " ++ s

instance Error TCError where
    noMsg  = Fail ""
    strMsg = Fail

---------------------------------------------------------------------------
-- * The Monad
---------------------------------------------------------------------------

type DbgMsg = String

newtype TC a = TC { unTC :: ReaderT TCEnv (ErrorT TCError (StateT TCState (Writer DbgMsg))) a }
    deriving (Functor, MonadReader TCEnv, MonadState TCState, MonadError TCError, MonadWriter DbgMsg, Monad)

instance Applicative TC where
  pure  = return
  (<*>) = ap

runTC :: TC a -> (Either TCError a, DbgMsg)
runTC (TC m) = runWriter . flip evalStateT initState . runErrorT . flip runReaderT emptyEnv $ m

runTC' :: TC a -> (TCEnv -> TCState -> ((Either TCError a, TCState), DbgMsg))
runTC' (TC m) e s = runWriter . flip runStateT s . runErrorT . flip runReaderT e $ m