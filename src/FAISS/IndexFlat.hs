{-# LANGUAGE ForeignFunctionInterface #-}

module FAISS.IndexFlat
  ( indexFlatNew
  , indexFlatL2New
  , indexFlatIPNew
  , indexRefineFlatNew
  , indexFlat1DNew
  , indexFlat1DNewWith
  , indexFlat1DUpdatePermutation
  , indexFlatComputeDistanceSubset
  ) where

import FAISS.Internal.Index
import FAISS.Internal.IndexFlat
import Foreign
import FAISS.Internal.Utils
import Data.Maybe (listToMaybe, fromMaybe)

-- | Create a new IndexFlat with given dimension and metric.
indexFlatNew :: Int -> FaissMetricType -> IO (Either String FaissIndex)
indexFlatNew d metric
  | d < 1 = pure $ Left "Dimension must be positive"
  | otherwise =
      alloca $ \ptr -> do
        ret <-
          c_faiss_IndexFlat_new_with
            ptr
            (fromIntegral d)
            (fromIntegral $ fromEnum metric)
        if ret == 0
          then Right . castPtr <$> peek ptr
          else return $ Left "Failed to create IndexFlat"

-- | Create a new IndexFlatL2 (L2 metric is implicit)
indexFlatL2New :: Int -> IO (Either String FaissIndex)
indexFlatL2New d
  | d < 1 = pure $ Left "Dimension must be positive"
  | otherwise =
      alloca $ \ptr -> do
        ret <- c_faiss_IndexFlatL2_new_with ptr (fromIntegral d)
        if ret == 0
          then Right . castPtr <$> peek ptr
          else return $ Left "Failed to create IndexFlatL2"

-- | Create a new IndexFlatIP (inner product metric)
indexFlatIPNew :: Int -> IO (Either String FaissIndex)
indexFlatIPNew d
  | d < 1 = pure $ Left "Dimension must be positive"
  | otherwise =
      alloca $ \ptr -> do
        ret <- c_faiss_IndexFlatIP_new_with ptr (fromIntegral d)
        if ret == 0
          then Right . castPtr <$> peek ptr
          else return $ Left "Failed to create IndexFlatIP"

-- | Create an IndexRefineFlat wrapping another index
indexRefineFlatNew :: FaissIndex -> IO (Either String FaissIndex)
indexRefineFlatNew baseIndex =
  alloca $ \ptr -> do
    ret <- c_faiss_IndexRefineFlat_new ptr baseIndex
    if ret == 0
      then Right . castPtr <$> peek ptr
      else return $ Left "Failed to create IndexRefineFlat"

-- | Create IndexFlat1D (default with continuous_update = 0)
indexFlat1DNew :: IO (Either String FaissIndex)
indexFlat1DNew =
  alloca $ \ptr -> do
    ret <- c_faiss_IndexFlat1D_new ptr
    if ret == 0
      then Right . castPtr <$> peek ptr
      else return $ Left "Failed to create IndexFlat1D"

-- | Create IndexFlat1D with control over continuous update flag
indexFlat1DNewWith :: Bool -> IO (Either String FaissIndex)
indexFlat1DNewWith continuousUpdate =
  alloca $ \ptr -> do
    let flag = if continuousUpdate then 1 else 0
    ret <- c_faiss_IndexFlat1D_new_with ptr flag
    if ret == 0
      then Right . castPtr <$> peek ptr
      else return $ Left "Failed to create IndexFlat1D (with)"

-- | Update permutation for IndexFlat1D
indexFlat1DUpdatePermutation :: FaissIndex -> IO (Either String ())
indexFlat1DUpdatePermutation index = do
  let flat1D = castPtr index :: FaissIndexFlat1D
  ret <- c_faiss_IndexFlat1D_update_permutation flat1D
  return $ if ret == 0 then Right () else Left "Failed to update permutation"

-- | Compute distances between query vectors and indexed vectors by label
indexFlatComputeDistanceSubset ::
  FaissIndex ->
  -- | Query vectors
  [[Float]] ->
  -- | Labels: size n * k
  [[Int]] ->
  IO (Either String [[Float]])
indexFlatComputeDistanceSubset index queries labels
  | length queries /= length labels =
      pure $ Left "Queries and labels must have same outer length"
  | otherwise = do
      let n = length queries
          k = length (fromMaybe [] $ listToMaybe labels)
          queryFlat = concat queries
          labelFlat = concat labels
      withFloatArray queryFlat $ \qptr ->
        withIdxArray labelFlat $ \lptr ->
          allocaArray (n * k) $ \dptr -> do
            ret <-
              c_faiss_IndexFlat_compute_distance_subset
                index
                (fromIntegral n)
                qptr
                (fromIntegral k)
                dptr
                lptr
            if ret == 0
              then do
                ds <- peekFloatArray (n * k) dptr
                return $ Right (chunksOf k ds)
              else return $ Left "Failed to compute distance subset"
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf m xs = let (h, t) = splitAt m xs in h : chunksOf m t
