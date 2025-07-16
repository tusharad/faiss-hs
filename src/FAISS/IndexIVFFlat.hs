{-# LANGUAGE ForeignFunctionInterface #-}

module FAISS.IndexIVFFlat
  ( indexIVFFlatNew
  , indexIVFFlatNewWith
  , indexIVFFlatNewWithMetric
  , indexIVFFlatAddCore
  , indexIVFFlatUpdateVectors
  , indexIVFFlatSetNProbe
  ) where

import FAISS.Internal.Index
import FAISS.Internal.IndexIVFFlat
import FAISS.Internal.Utils
import Foreign

-- | Create an empty IVFFlat index
indexIVFFlatNew :: IO (Either String FaissIndex)
indexIVFFlatNew = alloca $ \ptr -> do
  ret <- c_faiss_IndexIVFFlat_new ptr
  if ret == 0
    then Right . castPtr <$> peek ptr
    else return $ Left "Failed to create IndexIVFFlat"

-- | Create IVFFlat with quantizer, dimension, and nlist
indexIVFFlatNewWith :: FaissIndex -> Int -> Int -> IO (Either String FaissIndex)
indexIVFFlatNewWith quantizer d nlist = alloca $ \ptr -> do
  ret <-
    c_faiss_IndexIVFFlat_new_with
      ptr
      quantizer
      (fromIntegral d)
      (fromIntegral nlist)
  if ret == 0
    then Right . castPtr <$> peek ptr
    else return $ Left "Failed to create IndexIVFFlat (with quantizer)"

-- | Create IVFFlat with quantizer, dimension, nlist, and metric
indexIVFFlatNewWithMetric ::
  FaissIndex ->
  Int ->
  Int ->
  FaissMetricType ->
  IO (Either String FaissIndex)
indexIVFFlatNewWithMetric quantizer d nlist metric = alloca $ \ptr -> do
  ret <-
    c_faiss_IndexIVFFlat_new_with_metric
      ptr
      quantizer
      (fromIntegral d)
      (fromIntegral nlist)
      (fromIntegral $ fromEnum metric)
  if ret == 0
    then Right . castPtr <$> peek ptr
    else return $ Left "Failed to create IndexIVFFlat (with metric)"

-- | Add vectors with optional precomputed assignments
indexIVFFlatAddCore ::
  FaissIndex ->
  [[Float]] ->
  Maybe [Int64] ->
  Maybe [Int] ->
  IO (Either String ())
indexIVFFlatAddCore idx vectors precomputed maybeIds = do
  let n = fromIntegral $ length vectors
      flatVecs = concat vectors
  withFloatArray flatVecs $ \vecPtr ->
    withOptionalArray (fmap (map fromIntegral) maybeIds) $ \xidPtr ->
      withOptionalArray precomputed $ \prePtr -> do
        ret <- c_faiss_IndexIVFFlat_add_core (castPtr idx) n vecPtr xidPtr prePtr
        return $
          if ret == 0
            then
              Right ()
            else Left "Failed to add vectors using add_core"

-- | Update a subset of vectors
-- | Note: The index must have a direct_map
indexIVFFlatUpdateVectors ::
  FaissIndex ->
  [Int] ->
  [[Float]] ->
  IO (Either String ())
indexIVFFlatUpdateVectors idx ids newVecs = do
  let nv = length ids
      flatVecs = concat newVecs
  withIdxArray ids $ \idPtr ->
    withFloatArray flatVecs $ \vecPtr -> do
      ret <-
        c_faiss_IndexIVFFlat_update_vectors
          (castPtr idx)
          (fromIntegral nv)
          idPtr
          vecPtr
      return $ if ret == 0 then Right () else Left "Failed to update vectors"

-- | Set nprobe for IVFFlat
indexIVFFlatSetNProbe :: FaissIndex -> Int -> IO (Either String ())
indexIVFFlatSetNProbe idx nprobe = do
  let casted = castPtr idx :: FaissIndexIVFFlat
  c_faiss_IndexIVFFlat_set_nprobe casted (fromIntegral nprobe)
  return $ Right ()
