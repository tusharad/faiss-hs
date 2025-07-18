module Main (main) where

import FAISS.Index
import FAISS.Internal.Index
import FAISS.Internal.IndexFactory
-- import Flat
-- import IVFFlat
import Sample

main :: IO ()
main = do
  runApp
  {-
  let d = 32
  eRes <- indexFactory d "Flat" MetricL2
  case eRes of
    Left err -> putStrLn $ "error: " <> err
    Right faissIndex -> do
      indexGetD faissIndex >>= print
      indexGetIsTrained faissIndex >>= print
      indexGetNTotal faissIndex >>= print
      indexGetMetricType faissIndex >>= print
      let n = 1000
          q = 100
      let databaseMatrix = replicate n (replicate d 2.3)
      let queryMatrix = replicate q (replicate d 3.3)
      print (take 3 databaseMatrix)
      _ <- indexAdd faissIndex databaseMatrix
      indexGetNTotal faissIndex >>= print
      indexSearch faissIndex queryMatrix 3 >>= (\x -> print $ fst <$> x)
      putStrLn "all good"
      -}
