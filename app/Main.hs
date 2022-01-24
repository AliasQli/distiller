module Main where

import           Bathe
import           Codec.Serialise
import           Control.Monad
import qualified Data.Text.IO       as T
import           Distill
import           System.Environment
import           System.Random

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-d", text, wisdom]        -> d text wisdom
    ["--distill", text, wisdom] -> d text wisdom
    ["-b", wisdom]              -> b wisdom "20"
    ["-b", wisdom, count]       -> b wisdom count
    ["--bathe", wisdom]         -> b wisdom "20"
    ["--bathe", wisdom, count]  -> b wisdom count
    _ -> do
      putStrLn "Usage:"
      putStrLn "  -d, --distill <text>   <wisdom>"
      putStrLn "  -b, --bathe   <wisdom> [count=20]"
  where
    d text wisdom = do
      t <- T.readFile text
      writeFileSerialise wisdom $ distill t
    b wisdom count = do
      let c :: Int = read count
      w <- readFileDeserialise wisdom  
      forM_ [1..c] $ \_ -> newStdGen >>= T.putStrLn . bathe w
