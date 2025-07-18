{-# LANGUAGE ScopedTypeVariables #-}

module Flat (runApp) where

import System.Random (randomRIO)
import Control.Monad (replicateM)

import FAISS.Index
import FAISS.IndexFlat

runApp :: IO ()
runApp = do
  let d = 64      -- dimension
      nb = 100000 -- database size
      nq = 10000  -- number of queries
      k = 4       -- number of nearest neighbors to retrieve

  -- Generate reproducible database and query vectors
  xb <- genVectors nb d True
  xq <- genVectors nq d True

  -- Create IndexFlatL2
  indexE <- indexFlatL2NewWith d
  case indexE of
    Left err -> putStrLn $ "Failed to create index: " ++ err
    Right index -> do

      -- Check if index is trained
      trained <- indexGetIsTrained index
      putStrLn $ "Is trained? " ++ show trained

      -- Add database vectors
      _ <- indexAdd index xb

      -- Print total number of vectors in the index
      total <- indexGetNTotal index
      putStrLn $ "ntotal = " ++ show total

      -- Search top-k neighbors for a small batch (sanity check)
      sanityRes <- indexSearch index (take 5 xb) k
      case sanityRes of
        Left err -> putStrLn $ "Search failed: " ++ err
        Right (distances, labels) -> do
          putStrLn "Sanity check labels (xb[:5]):"
          print $ chunksOf k labels
          putStrLn "Sanity check distances:"
          print $ chunksOf k distances

      -- Full query
      fullRes <- indexSearch index xq k
      case fullRes of
        Left err -> putStrLn $ "Search failed: " ++ err
        Right (_, labels) -> do
          putStrLn "First 5 query results:"
          print $ take 5 (chunksOf k labels)
          putStrLn "Last 5 query results:"
          print $ take 5 (reverse $ chunksOf k labels)

-- Generate reproducible random vectors (add increasing offset to dimension 0)
genVectors :: Int -> Int -> Bool -> IO [[Float]]
genVectors n d offsetFirst =
  mapM (\i -> do
          vec <- replicateM d (randomRIO (0.0, 1.0 :: Float))
          let vec' = if offsetFirst then addOffset i vec else vec
          return vec') [0 .. n - 1]
  where
    addOffset i (x:xs) = (x + fromIntegral i / 1000) : xs
    addOffset _ [] = []

-- Split a flat list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t
