{-# LANGUAGE OverloadedLists #-}

module IVFFlat (runApp) where

import qualified FAISS.Index as FI
import qualified FAISS.IndexFlat as FI
import qualified FAISS.IndexIVFFlat as IVFF
import System.Random

-- Helper to generate random float vectors
randomVectors :: Int -> Int -> IO [[Float]]
randomVectors n d = do
  g <- newStdGen
  let floats = take (n * d) (randoms g :: [Float])
      vectors = chunksOf d floats
  return $
    zipWith
      (\i v -> (head v + i / 1000) : drop 1 v)
      [0 ..]
      vectors

runApp :: IO ()
runApp = do
  let d = 64
      nb = 100000
      nq = 10000
      nlist = 100
      k = 4

  putStrLn "Generating database vectors..."
  xb <- randomVectors nb d
  putStrLn "Generating query vectors..."
  xq <- randomVectors nq d

  -- Create quantizer (FlatL2 index)
  putStrLn "Creating FlatL2 quantizer..."
  quantizerResult <- FI.indexFlatL2NewWith d
  quantizer <- case quantizerResult of
    Left err -> error err
    Right q -> return q

  -- Create IVF index
  putStrLn "Creating IVFFlat index..."
  ivfResult <- IVFF.indexIVFFlatNewWithMetric quantizer d nlist FI.MetricL2
  index <- case ivfResult of
    Left err -> error err
    Right idx -> return idx

  trained <- FI.indexGetIsTrained index
  putStrLn $ "Is trained? " ++ show trained

  putStrLn "Training index..."
  trainResult <- FI.indexTrain index xb
  case trainResult of
    Left err -> error err
    Right _ -> return ()

  trained' <- FI.indexGetIsTrained index
  putStrLn $ "Is trained after training? " ++ show trained'

  putStrLn "Adding vectors to index..."
  addResult <- FI.indexAdd index xb
  case addResult of
    Left err -> error err
    Right _ -> return ()

  putStrLn "Searching before setting nprobe..."
  res1 <- FI.indexSearch index xq k
  case res1 of
    Left err -> error err
    Right (_, labels) -> do
      let last5 = drop (length labels - 5 * k) labels
      putStrLn "Neighbors of the last 5 queries (nprobe=1):"
      print (chunksOf k last5)

  putStrLn "Setting nprobe to 10..."
  _ <- IVFF.indexIVFFlatSetNProbe index 10

  putStrLn "Searching after setting nprobe..."
  res2 <- FI.indexSearch index xq k
  case res2 of
    Left err -> error err
    Right (_, labels2) -> do
      let last5' = drop (length labels2 - 5 * k) labels2
      putStrLn "Neighbors of the last 5 queries (nprobe=10):"
      print (chunksOf k last5')

-- Split a flat list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t
