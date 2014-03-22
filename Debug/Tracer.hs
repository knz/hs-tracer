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
-- Transformers for Functor/Applicative/Monad types that add
-- tracing facilities for "computation steps" and applicative nesting.
---------------------------------------------------------------------

module Debug.Tracer (
         Pos
       , PosShort, PosRel, PosFrame, PosStack
       , Tracer, label, trace, enter
       , TracerTrans, runTracerT
       , TracerT
       , runTracer
) where

import qualified Debug.Trace

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- Position: where the "execution cursor" is currently at.
-- Identified by a label and a "number of steps" since the label
-- position was encountered.

class Pos p where
   -- initial position
   pinitial :: String -> p
   -- set current label
   plabel :: String -> p -> p
   -- step: make one step forward
   pstep :: p -> p
   -- rewind: make one step back
   prewind :: p -> p
   -- enter a scope
   ppush :: String -> p -> p
   -- restore after a scope leaves. 1st argument
   -- is current (caller) position, 2nd argument
   -- is final position in callee.
   ppop :: p -> p -> p
   -- trace: output message with position as prefix
   ptrace :: p -> String -> a -> a


class (Applicative m) => Tracer m where
  label :: String -> m ()
  trace :: String -> m ()
  enter :: m a -> m a

class TracerTrans t where
  runTracerT :: (Applicative m) => String -> t m a -> m a

-- In a TracerT programs can use  "enter", "label" and "trace" (defined below)
-- for "print debugging"


data PosShort = Ps Int
data PosRel = Pr Int Int
data PosFrame = Pf Int String Int
data PosStack = Pst Int String String Int

dotrace :: String -> String -> a -> a
dotrace pref msg = Debug.Trace.trace (pref ++ ": " ++ msg)

instance Pos PosShort where
   pinitial _ = Ps 0
   plabel _ = id
   pstep (Ps n) = Ps (n+1)
   prewind (Ps n) = Ps (n-1)
   ppush _ = id
   ppop _ = id
   ptrace (Ps n) = dotrace (show n)

instance Pos PosRel where
   pinitial _ = Pr 0 0
   plabel _ = id
   pstep (Pr n i) = Pr (n+1) (i+1)
   prewind (Pr n i) = Pr (n-1) (i-1)
   ppush _ (Pr n i) = Pr n 0
   ppop (Pr _ i) (Pr n' _) = Pr n' i
   ptrace (Pr n i) = dotrace ((show n) ++ " +" ++ (show i))

instance Pos PosFrame where
   pinitial w = Pf 0 w 0
   plabel _ = id
   pstep (Pf n l i) = Pf (n+1) l (i+1)
   prewind (Pf n l i) = Pf (n+1) l (i+1)
   ppush w (Pf n _ i) = Pf n w 0
   ppop (Pf _ l i) (Pf n' _ _) = Pf n' l i
   ptrace (Pf n w i) = dotrace ((show n) ++ " " ++ w ++ "+" ++ (show i))

instance Pos PosStack where
   pinitial w = Pst 0 w "" 0
   plabel w (Pst n c _ i) = Pst n c w i
   pstep (Pst n c l i) = Pst (n+1) c l (i+1)
   prewind (Pst n c l i) = Pst (n-1) c l (i-1)
   ppush w (Pst n c l i) = Pst n (c ++ " " ++ l ++ "+" ++ (show i) ++ ">") "" 0
   ppop (Pst _ c l i) (Pst n' _ _ _) = Pst n' c l i
   ptrace (Pst n c l i) = dotrace ((show n) ++ " " ++ c ++ " " ++ l ++ "+" ++ (show i))

-- TracerT: monad transformer.
-- Turns a monad into one that traces execution step by step.

newtype TracerT p m a = TracerT { stepfunc :: (p -> m (a, p)) }

instance (Functor m, Pos p) => Functor (TracerT p m) where
  -- fmap :: (a -> b) -> m a -> m b
  fmap f (TracerT x) = TracerT $ \l -> let next = x l
                                           trans (v, l') = (f v, pstep l')
                                       in  fmap trans next

instance (Applicative m, Pos p) => Applicative (TracerT p m) where
  -- pure :: a -> m a
  pure x                      = TracerT $ seq x $ \l -> pure (x, pstep l)

  -- (<*>) :: f (a -> b) -> f a -> f b
  (TracerT f) <*> (TracerT x) = TracerT $ seq (seq f x) $ \l ->
        let fnext        = f (ppush "<_*>" l)    -- :: m (a -> b, p)
            trans (f, l) = \(x,l') -> (f x, ppop l (pstep l')) -- :: (a->b,p) -> ((a,p)->(b,p))
            fenc         = fmap trans fnext     -- :: m ((a,p)->(b,p))
            xnext        = x (ppush "<*_>" l)          -- :: m (a, p)
        in  fenc <*> xnext

instance (Monad m, Pos p) => Monad (TracerT p m) where
  return x          = TracerT $ seq x $ \p -> return (x, pstep p)
  (TracerT x) >>= f = TracerT $         \p -> do
                                              (v, p') <- x p
                                              (TracerT x') <- return $ f v
                                              x' (pstep p')





instance (Pos p) => MonadTrans (TracerT p) where
  lift x = TracerT $ seq x $ \p -> do
                                    v <- x
                                    return (v, pstep p)


instance (Pos p, Applicative m) => Tracer (TracerT p m) where
  label lbl = TracerT $ \p -> pure ( (), plabel lbl p)
  trace msg = TracerT $ \p -> ptrace p msg $ pure ( (), prewind p )
  enter x = (pure id) <*> x

instance (Pos p) => TracerTrans (TracerT p) where
  runTracerT w (TracerT x) = fmap fst (x (pinitial w))


-- runTracerT: executes a traceT program in a pure environment.
runTracer :: (Pos p) => String -> TracerT p Identity a -> a
runTracer w = runIdentity . (runTracerT w)
