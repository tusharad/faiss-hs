{-# LANGUAGE OverloadedStrings #-}

module Sample (runApp) where

import Control.Monad (forM, forM_, replicateM)
import qualified System.Random as R
import Text.Printf (printf)

import FAISS
import qualified FAISS.AuxIndexStructures as Aux
import qualified FAISS.Index as Index
import qualified FAISS.IndexFactory as Factory

runApp :: IO ()
runApp = do
  let d = 128
      nb = 100000
      nq = 10000
      k = 5

  putStrLn "Generating some data..."

  xb <- generateData nb d True
  xq <- generateData nq d False

  putStrLn "Building an index..."

  Right index <- Factory.indexFactory d "Flat" Index.MetricL2

  trained <- Index.indexGetIsTrained index
  putStrLn $ "is_trained = " ++ show trained

  Right () <- Index.indexAdd index xb
  total <- Index.indexGetNTotal index
  putStrLn $ "ntotal = " ++ show total

  putStrLn "Searching..."

  -- 1. Search xb[:5]
  runSearch index (take 5 xb) k "Search: xb[:5]"

  -- 2. Search xq
  runSearch index xq k "Search: xq"

  -- 3. Search xq with IDSelectorRange [50,100]
  runSearchWithRange index xq k 50 100 "Search: xq w/ IDSelectorRange [50, 100]"

  -- 4. IDSelectorOr [20,40] OR [45,60]
  runSearchWithOr
    index
    xq
    k
    (20, 40)
    (45, 60)
    "Search: xq w/ IDSelectorRange [20,40] OR [45,60]"

  -- 5. IDSelectorAnd [20,40] AND [15,35] = [20,35]
  runSearchWithAnd
    index
    xq
    k
    (20, 40)
    (15, 35)
    "Search: xq w/ IDSelectorRange [20,40] AND [15,35]"

  putStrLn "Saving index to disk..."
  Right () <- Index.indexFileWrite index "example.index"

  putStrLn "Freeing index..."
  Index.indexFree index

  putStrLn "Done."

-- Generate synthetic data
generateData :: Int -> Int -> Bool -> IO [[Float]]
generateData n d _ =
  forM [0 .. n - 1] $ \i -> do
    vec <- replicateM d randomFloat
    let modified = (head vec + fromIntegral i / 1000) : tail vec
    return modified

randomFloat :: IO Float
randomFloat = R.randomRIO (0, 1)

-- Basic search and print
runSearch :: FaissIndex -> [[Float]] -> Int -> String -> IO ()
runSearch index queries k label = do
  Right (dists, labels) <- Index.indexSearch index queries k
  putStrLn label
  printTop5 dists labels k

-- Search with IDSelectorRange
runSearchWithRange :: FaissIndex -> [[Float]] -> Int -> Int -> Int -> String -> IO ()
runSearchWithRange index queries k start end label = do
  Right sel <- Aux.idSelectorRangeNew start end
  Right params <- Index.searchParametersNew sel
  Right (dists, labels) <- Index.indexSearchWithParams index queries k params
  putStrLn label
  printTop5 dists labels k
  Index.searchParametersFree params
  Index.idSelectorRangeFree sel

-- Search with IDSelectorOr
runSearchWithOr ::
  FaissIndex ->
  [[Float]] ->
  Int ->
  (Int, Int) ->
  (Int, Int) ->
  String ->
  IO ()
runSearchWithOr index queries k (a1, a2) (b1, b2) label = do
  Right sel1 <- Aux.idSelectorRangeNew a1 a2
  Right sel2 <- Aux.idSelectorRangeNew b1 b2
  Right orSel <- Aux.idSelectorOrNew sel1 sel2
  Right params <- Index.searchParametersNew orSel
  Right (dists, labels) <- Index.indexSearchWithParams index queries k params
  putStrLn label
  printTop5 dists labels k
  Index.searchParametersFree params
  Index.idSelectorRangeFree sel1
  Index.idSelectorRangeFree sel2
  Index.idSelectorFree orSel

-- Search with IDSelectorAnd
runSearchWithAnd ::
  FaissIndex ->
  [[Float]] ->
  Int ->
  (Int, Int) ->
  (Int, Int) ->
  String ->
  IO ()
runSearchWithAnd index queries k (a1, a2) (b1, b2) label = do
  Right sel1 <- Aux.idSelectorRangeNew a1 a2
  Right sel2 <- Aux.idSelectorRangeNew b1 b2
  Right andSel <- Aux.idSelectorAndNew sel1 sel2
  Right params <- Index.searchParametersNew andSel
  Right (dists, labels) <- Index.indexSearchWithParams index queries k params
  putStrLn label
  printTop5 dists labels k
  Index.searchParametersFree params
  Index.idSelectorRangeFree sel1
  Index.idSelectorRangeFree sel2
  Index.idSelectorFree andSel

-- Helper to print top 5 results
printTop5 :: [Float] -> [Int] -> Int -> IO ()
printTop5 dists labels k =
  forM_ [0 .. 4] $ \i -> do
    forM_ [0 .. k - 1] $ \j -> do
      let idx = i * k + j
      printf "%5d (d=%.3f)  " (labels !! idx) (dists !! idx)
    putStrLn ""
