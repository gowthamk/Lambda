module StateM where
 
  {- from mtl:
  import Control.Monad.State.Lazy
  -}
  {- from transformers: -}
  import Control.Monad.Trans.State
  import Control.Monad.Trans (liftIO)
  import Control.Exception (throw, AsyncException(UserInterrupt))

  type StateIO = StateT (Int,Int) IO

  invoke :: (Int -> Int) -> StateIO ()
  invoke f = do
    (st,i) <- get
    liftIO $ putStrLn $ "invoking "++(show (i+1))++"'th op"
    let st' = f st
    put (st',i+1)

  runStateIO :: StateIO a -> Int -> IO a
  runStateIO sm v = do
    putStrLn "Running the state-passing computation"
    throw UserInterrupt 
    (a,(st,i)) <- runStateT sm (v,0)
    putStrLn $ (show i)++" operations on state have been executed"
    putStrLn $ "The final state is "++(show st)
    return a
 
  (<+>) :: StateIO () -> StateIO () -> StateIO ()
  (<+>) sm1 sm2 = sm1 >>= (\_ -> sm2)

  main :: IO ()
  main = do
    let op1 = (\i -> i+1)
    let op2 = (\i -> 2*i)
    let sm1 = invoke op1
    let sm2 = invoke op2
    let sm = sm1 <+> sm2
    runStateIO sm 3

{- Sample Execution
*StateM > :main
Running the state-passing computation
invoking 1'th op
invoking 2'th op
2 operations on state have been executed
The final state is 8
-}
