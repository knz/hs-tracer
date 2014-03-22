Tracing utilities for Haskell code
==================================

This library contains **Debug.Tracer**, a module that provides some
support for "print debugging" of Haskell code, and *even for pure code*: no
explicit I/O typing is required.

Note: the file defining this documentation (``README.lhs``) is itself
a Haskell program, which can be run for testing the library.

Example
-------

The following code defines a function ``myfact`` that computes
the factorial of an integer number::

> import Debug.Tracer
>
> -- Create a shortcut for 'PureTracer PostStack',
> -- a tracer for pure computations able to track
> -- labeled basic blocks in a monadic context (eg. do-blocks)
> type Tr a = PureTracer PosStack a
>
> -- An example function using our tracer
> myfact :: Int -> Tr Int
> myfact n = do
>              trace $ "entering with n = " ++ (show n)
>              if n == 1 then
>                 return n
>              else do
>                 r <- enter $ myfact (n - 1)
>                 trace $ "just computed r = " ++ (show r)
>                 return (n * r)
>
> -- The function can be used in a pure context, eg:
> myfact' :: Int -> Int
> myfact' = (runTracer "myfact") . myfact

As the example demonstrates, the **Tracer** modules allows a
programmer to write pure code in a semi-imperative style, with the
``trace`` and ``enter`` actions helping to report execution progress.

For example, if the code above is extended to become
a fully-fledged program as follows::

>
> main :: IO ()
> main = do
>           putStrLn $ show $ myfact' 10

Then this program would print a trace like the following::

   1 myfact +1: entering with n = 10
   2 myfact +1> +1: entering with n = 9
   3 myfact +1> +1> +1: entering with n = 8
   4 myfact +1> +1> +1> +1: entering with n = 7
   5 myfact +1> +1> +1> +1> +1: entering with n = 6
   6 myfact +1> +1> +1> +1> +1> +1: entering with n = 5
   7 myfact +1> +1> +1> +1> +1> +1> +1: entering with n = 4
   8 myfact +1> +1> +1> +1> +1> +1> +1> +1: entering with n = 3
   9 myfact +1> +1> +1> +1> +1> +1> +1> +1> +1: entering with n = 2
   10 myfact +1> +1> +1> +1> +1> +1> +1> +1> +1> +1: entering with n = 1
   13 myfact +1> +1> +1> +1> +1> +1> +1> +1> +1> +3: just computed r = 1
   16 myfact +1> +1> +1> +1> +1> +1> +1> +1> +3: just computed r = 2
   19 myfact +1> +1> +1> +1> +1> +1> +1> +3: just computed r = 6
   22 myfact +1> +1> +1> +1> +1> +1> +3: just computed r = 24
   25 myfact +1> +1> +1> +1> +1> +3: just computed r = 120
   28 myfact +1> +1> +1> +1> +3: just computed r = 720
   31 myfact +1> +1> +1> +3: just computed r = 5040
   34 myfact +1> +1> +3: just computed r = 40320
   37 myfact +1> +3: just computed r = 362880
   3628800




.. code:: haskell

 import Debug.Tracer
 import qualified Debug.Trace (trace)
 import Control.Applicative (Applicative(..))
 import Control.Monad.Trans.Class (MonadTrans, lift)

 type MT a = IOTracer PosStack a

 mycode :: MT Int
 mycode = do
         label "mycode"
         trace "start"
         x <- return $ Debug.Trace.trace "some3" 3
         trace "between"
         y <- return $ Debug.Trace.trace "some4" 4
         trace "end"
         return $ Debug.Trace.trace "some+" (x + y)

 fact n = if n == 1 then 1 else n * fact (n-1)

 myfact ::  Int -> MT Int
 myfact n = do
      label "myfact"
      trace $ "fact " ++ (show n)
      if n == 1 then return n
      else do
           r <- enter $ myfact (n - 1)
           return (n * r)

 myfact2 :: Int -> Int -> MT Int
 myfact2 r n = do
      label "myfact2"
      trace $ "fact2 " ++ (show n)
      if n == 1 then return r
      else myfact2 (n * r) (n - 1)

 main = do
       putStrLn "at the early beginning"
       v <- runTracerT "top" $ do
                 trace "before-mycode"
                 t <- enter mycode;
                 trace "after-mycode"
                 label "facttest"
                 v <- enter $ myfact 10
                 v' <- enter $ myfact2 1 10
                 trace "after-fact"
                 lift $ putStrLn $ "IO actions are allowed" ++ (show (v+v'))
                 trace "after-io"
                 tv <- (pure fact) <*> mycode
                 trace "more fact"

                 return (t + tv)
       putStrLn "all said and done"
       putStrLn . show $ v
