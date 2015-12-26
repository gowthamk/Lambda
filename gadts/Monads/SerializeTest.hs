module SerializeTest where

  import Data.ByteString (ByteString)
  import Data.Serialize (encode,decode)
  import Control.Exception (evaluate)

  foo :: ByteString -> Either String Bool
  foo x = decode x

  main :: IO ()
  main = do
    let x = encode ()
    let y = foo x
    esb <- evaluate y
    case esb of 
      Left str -> putStrLn str
      Right _ -> return ()
