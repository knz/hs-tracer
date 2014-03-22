---------------------------------------------------------------------
-- |
-- Module      : Debug.Tracer
-- Copyright   : (c) Raphael 'kena' Poss 2014
-- License     : BSD3
--
-- Maintainer  : kena@vodka-pomme.net
-- Stability   : experimental
-- Portability : portable
--
-- Transformers for 'Functor', 'Applicative' and 'Monad' types that add
-- tracing facilities for computation steps and applicative nesting.
---------------------------------------------------------------------

module Debug.Tracer (
       -- * General interfaces
         TracerTrans(runTracerT)
       , Tracer(label, trace, enter)
       -- * Position types
       , Pos
       , PosShort, PosRel, PosStack
       -- * Tracer transformer
       , TracerT
       -- * Utilities
       , PureTracer
       , runTracer
       , MaybeTracer, IOTracer
) where

import qualified Debug.Trace

import Control.Applicative (Applicative(..))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadTrans(..))

-------------------------------------------------------------------
-- General interfaces
--

-- |
-- Tracer transformers are Applicative transformers.
-- All resulting tracers can be evaluated to trace the evaluation
-- of the applicative effects that they encapsulate.

class TracerTrans t where

  -- |
  -- Evaluate the tracer, which forces the application
  -- and reports the progress according to the uses of
  -- 'trace', 'label' and 'enter' in the composition.
  runTracerT :: (Applicative m) => String -> t m a -> m a


-- |
-- A tracer structure internally tracks the progress of 'Applicative'
-- computations performed “within it”.
--
class (Applicative m) => Tracer m where

  -- |
  -- Emit the current progress followed by a message. The progress
  -- is emitted using the standard 'Debug.Trace.trace' function.
  trace :: String -> m ()

  -- |
  -- Label the current computation step, so that subsequent uses
  -- of 'trace' will report the relative progress since this step.
  label :: String -> m ()

  -- |
  -- Mark a computation as a “call” (nesting), so that any
  -- relative progress counters are restored when the nested
  -- computation ends.
  enter :: m a -> m a



---------------------------------------------------------------------
-- Position types


-- |
-- The class of position holders for 'TracerT' instances.  There are
-- multiple types in this class in order with different levels of
-- complexity, so that user programs can control the amount of
-- overhead involved during tracing.

class Pos p where
   -- initial position
   pinitial :: String -> p
   -- set current label
   plabel   :: String -> p -> p
   -- step: make one step forward
   pstep    :: p -> p
   -- rewind: make one step back
   prewind  :: p -> p
   -- enter a scope
   ppush    :: p -> p
   -- restore after a scope leaves. 1st argument
   -- is current (caller) position, 2nd argument
   -- is final position in callee.
   ppop     :: p -> p -> p
   -- trace: output message with position as prefix
   ptrace   :: p -> String -> a -> a

-- |
-- A lightweight position type that only records the global
-- number of steps.

data PosShort = Ps !Int

-- |
-- A position type that extends 'PosShort' by also tracking
-- the relative number of steps
-- since the beginning of the current application group.

data PosRel = Pr !Int !Int

-- |
-- A position type that extends 'PosRel' by also tracking the name
-- of labels and the stacking of application levels.
--
-- It involves more run-time overhead due to string manipulations.

data PosStack = Pst !Int String String !Int


-- The Pos instances follow.

dotrace :: String -> String -> a -> a
dotrace pref msg = Debug.Trace.trace (pref ++ ": " ++ msg)

instance Pos PosShort where
   pinitial _      = Ps 0
   plabel   _      = id
   pstep    (Ps n) = Ps (n+1)
   prewind  (Ps n) = Ps (n-1)
   ppush           = id
   ppop     _      = id
   ptrace   (Ps n) = dotrace (show n)

instance Pos PosRel where
   pinitial _                  = Pr 0 0
   plabel   _                  = id
   pstep    (Pr n i)           = Pr (n+1) (i+1)
   prewind  (Pr n i)           = Pr (n-1) (i-1)
   ppush    (Pr n _)           = Pr n 0
   ppop     (Pr _ i) (Pr n' _) = Pr n' i
   ptrace   (Pr n i)           = dotrace ((show n) ++ " +" ++ (show i))

instance Pos PosStack where
   pinitial  w                            = Pst 0 w "" 0
   plabel    w (Pst n c _ i)              = Pst n c w i
   pstep     (Pst n c l i)                = Pst (n+1) c l (i+1)
   prewind   (Pst n c l i)                = Pst (n-1) c l (i-1)
   ppush     (Pst n c l i)                = Pst n (c ++ " " ++ l ++ "+" ++ (show i) ++ ">") "" 0
   ppop      (Pst _ c l i) (Pst n' _ _ _) = Pst n' c l i
   ptrace    (Pst n c l i)                = dotrace ((show n) ++ " " ++ c ++ " " ++ l ++ "+" ++ (show i))


---------------------------------------------------------------------
-- Tracer transformers.

-- |
-- Equips an underlying 'Functor', 'Applicative' or 'Monad' type
-- with tracing facilities.

newtype TracerT p m a = TracerT (p -> m (a, p))

-- |
-- Provides 'fmap' with tracing.

instance (Functor m, Pos p) => Functor (TracerT p m) where
  -- fmap :: (a -> b) -> m a -> m b
  fmap f (TracerT x) = TracerT $ \l -> let next = x l
                                           trans (v, l') = (f v, pstep l')
                                       in  fmap trans next

-- |
-- Provides sequencing with tracing ('<*>', '*>' and '<*').

instance (Applicative m, Pos p) => Applicative (TracerT p m) where
  -- pure :: a -> m a
  pure x                      = TracerT $ seq x $ \l -> pure (x, pstep l)

  -- (<*>) :: f (a -> b) -> f a -> f b
  (TracerT f) <*> (TracerT x) = TracerT $ seq (seq f x) $ \l ->
        let fnext                   = f (ppush l)          -- :: m (a -> b, p)

            trans (f', l') (x',l'') = (f' x', ppop l' (pstep l''))
                                                           -- :: (a->b,p) -> ((a,p)->(b,p))

            fenc                    = fmap trans fnext     -- :: m ((a,p)->(b,p))

            xnext                   = x (ppush l)          -- :: m (a, p)
        in  fenc <*> xnext

-- |
-- Provides do-notation with tracing.

instance (Monad m, Pos p) => Monad (TracerT p m) where
  return x          = TracerT $ seq x $ \p -> return (x, pstep p)
  (TracerT x) >>= f = TracerT $         \p -> do
                                              (v, p') <- x p
                                              (TracerT x') <- return $ f v
                                              x' (pstep p')




-- |
-- Provides 'lift', to trace actions from the underlying monad.

instance (Pos p) => MonadTrans (TracerT p) where
  lift x = TracerT $ seq x $ \p -> do
                                    v <- x
                                    return (v, pstep p)

{-
  -- FIXME: is this right?
  -- tmap :: (forall a. m a -> n a) -> (forall b. n b -> m b) -> t m c -> t n c
  tmap f _ x = TracerT $ \p -> do
                                 (v, p') <- x p
                                 (TracerT x') <- return $ f v
                                 x' (pstep p')
-}

-- |
-- Provides 'label', 'trace' and 'enter'.
instance (Pos p, Applicative m) => Tracer (TracerT p m) where
  label lbl = TracerT $ \p -> pure ( (), plabel lbl p)
  trace msg = TracerT $ \p -> ptrace p msg $ pure ( (), prewind p )
  enter x   = (pure id) <*> x

-- |
-- Provides 'runTracerT'.
instance (Pos p) => TracerTrans (TracerT p) where
  runTracerT w (TracerT x) = fmap fst (x (pinitial w))


-- |
-- Simple tracer for pure computations.
--
-- For this tracer, 'runTracerT' has the following type:
--
-- > runTracerT :: (Pos p) => String -> PureTracer p a -> Identity a
type PureTracer p a = TracerT p Identity a

-- |
-- Evaluates a traced pure computation encapsulated in a 'PureTracer',
-- emit its trace and
-- return the computation's result.
runTracer :: (Pos p) => String -> PureTracer p a -> a
runTracer w = runIdentity . (runTracerT w)

-- |
-- Simple tracer for 'Maybe' computations.
--
-- For this tracer, 'runTracerT' has the following type:
--
-- > runTracerT :: (Pos p) => String -> MaybeTracer p a -> Maybe a
type MaybeTracer p a = TracerT p Maybe a

-- |
-- Simple tracer for 'IO' computations.
--
-- For this tracer, 'runTracerT' has the following type:
--
-- > runTracerT :: (Pos p) => String -> IOTracer p a -> IO a
type IOTracer p a = TracerT p IO a
