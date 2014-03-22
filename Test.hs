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
