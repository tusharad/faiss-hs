{- |
Module      : FAISS.Index
Description : Module containing operations related to Index
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

High-level Haskell interface functions for Index
-}
module FAISS.Index
  ( searchParametersNew
  , indexGetD
  , indexGetIsTrained
  , indexGetNTotal
  , indexGetMetricType
  , indexGetVerbose
  , indexSetVerbose
  , indexTrain
  , indexAdd
  , indexAddWithIds
  , indexSearch
  , indexSearchWithParams
  , indexAssign
  , indexReset
  , indexRemoveIds
  , indexReconstruct
  , indexReconstructN
  , indexComputeResidual
  , indexComputeResidualN
  , indexSaCodeSize
  , indexSaEncode
  , indexSaDecode
  , FaissIDSelector
  , FaissSearchParameters
  , FaissIndex
  , FaissMetricType (..)
  , FaissRangeSearchResult
  , IdxT
  ) where

import FAISS.Internal.Index
import Foreign
import Foreign.C.Types

-- | Create new search parameters
searchParametersNew :: FaissIDSelector -> IO (Either String FaissSearchParameters)
searchParametersNew sel = do
  alloca $ \ptr -> do
    ret <- c_faiss_SearchParameters_new ptr sel
    if ret == 0
      then Right <$> peek ptr
      else return $ Left "Failed to create search parameters"

-- | Get the dimension of the index
indexGetD :: FaissIndex -> IO Int
indexGetD idx = fromIntegral <$> c_faiss_Index_d idx

-- | Check if the index is trained
indexGetIsTrained :: FaissIndex -> IO Bool
indexGetIsTrained idx = (/= 0) <$> c_faiss_Index_is_trained idx

-- | Get the total number of vectors in the index
indexGetNTotal :: FaissIndex -> IO Int
indexGetNTotal idx = fromIntegral <$> c_faiss_Index_ntotal idx

-- | Get the metric type of the index
indexGetMetricType :: FaissIndex -> IO FaissMetricType
indexGetMetricType idx = toEnum . fromIntegral <$> c_faiss_Index_metric_type idx

-- | Get the verbose level of the index (Either 0 or 1)
indexGetVerbose :: FaissIndex -> IO Int
indexGetVerbose idx = fromIntegral <$> c_faiss_Index_verbose idx

-- | Set the verbose level of the index (Either 0 or 1)
indexSetVerbose :: FaissIndex -> Int -> IO ()
indexSetVerbose idx level = c_faiss_Index_set_verbose idx (fromIntegral level)

-- | Perform training on a representative set of vectors
indexTrain ::
  FaissIndex ->
  -- | opaque pointer to index object
  [[Float]] ->
  -- | training vectors, size n * d; the function assumes each sub list is of length d
  IO (Either String ())
indexTrain idx vectors = do
  let n = fromIntegral $ length vectors
  let normalized_vector = concat vectors
  withFloatArray normalized_vector $ \ptr -> do
    ret <- c_faiss_Index_train idx n ptr
    return $ if ret == 0 then Right () else Left "Training failed"

{- | Add n vectors of dimension d to the index.
|  Vectors are implicitly assigned labels ntotal .. ntotal + n - 1
* This function slices the input vectors in chunks smaller than
-}
indexAdd ::
  FaissIndex ->
  -- | opaque pointer to index object
  [[Float]] ->
  -- |  input matrix, size n * d
  IO (Either String ())
indexAdd idx vectors = do
  let n = fromIntegral $ length vectors
  let normalized_vector = concat vectors
  withFloatArray normalized_vector $ \ptr -> do
    ret <- c_faiss_Index_add idx n ptr
    return $ if ret == 0 then Right () else Left "Adding vectors failed"

{- | Same as add, but stores xids instead of sequential ids.
|  The default implementation fails with an assertion, as it is
   not supported by all indexes.
-}
indexAddWithIds ::
  FaissIndex ->
  -- | opaque pointer to index object
  [[Float]] ->
  -- |  input matrix, size n * d
  [Int] ->
  -- | ids to store for the vectors (size n)
  IO (Either String ())
indexAddWithIds idx vectors ids = do
  let n = length vectors
  let normalized_vector = concat vectors
  if length ids /= n then pure $ Left "Length of ids should be same as n (number of vectors)"
  else do
    withFloatArray normalized_vector $ \vptr ->
      withIdxArray ids $ \iptr -> do
        ret <- c_faiss_Index_add_with_ids idx (fromIntegral n) vptr iptr
        return $ if ret == 0 then Right () else Left "Adding vectors with IDs failed"

{- | query n vectors of dimension d to the index.
    Search for nearest neighbors return at most k vectors.
    If there are not enough results for a query, the result array is padded with -1s.
-}
indexSearch ::
  FaissIndex ->
  -- | opaque pointer to index object
  [[Float]] ->
  -- | input vectors to search, size n * d
  Int ->
  -- | return at most k vectors
  IO (Either String ([Float], [Int]))
indexSearch idx queries k = do
  let n = fromIntegral $ length queries
  let resultSize = fromIntegral $ n * fromIntegral k
  let normalized_vector = concat queries
  withFloatArray normalized_vector $ \qptr ->
    allocaArray resultSize $ \dptr ->
      allocaArray resultSize $ \lptr -> do
        ret <- c_faiss_Index_search idx n qptr (fromIntegral k) dptr lptr
        if ret == 0
          then do
            distances <- peekFloatArray resultSize dptr
            labels <- peekIdxArray resultSize lptr
            return $ Right (distances, labels)
          else return $ Left "Search failed"

-- | query n vectors of dimension d with search parameters to the index.
indexSearchWithParams ::
  FaissIndex ->
  -- | opaque pointer to index object
  [[Float]] ->
  -- |  input vectors to search, size n * d
  Int ->
  -- | return at most k vectors.
  FaissSearchParameters ->
  -- | input params to modify how search is done
  IO (Either String ([Float], [Int]))
indexSearchWithParams idx queries k params = do
  let n = fromIntegral $ length queries
  let resultSize = fromIntegral $ n * fromIntegral k
  let normalized_vector = concat queries
  withFloatArray normalized_vector $ \qptr ->
    allocaArray resultSize $ \dptr ->
      allocaArray resultSize $ \lptr -> do
        ret <- c_faiss_Index_search_with_params idx n qptr (fromIntegral k) params dptr lptr
        if ret == 0
          then do
            distances <- peekFloatArray resultSize dptr
            labels <- peekIdxArray resultSize lptr
            return $ Right (distances, labels)
          else return $ Left "Search with parameters failed"

-- | return the indexes of the k vectors closest to the query x.
indexAssign ::
  FaissIndex ->
  -- |  opaque pointer to index object
  [[Float]] ->
  -- | input vectors to search, size n * d
  Int ->
  -- | at most k vectors
  IO (Either String [Int])
indexAssign idx vectors k = do
  let n = fromIntegral $ length vectors
  withFloatArray (concat vectors) $ \vptr ->
    allocaArray (fromIntegral n * k) $ \lptr -> do
      ret <- c_faiss_Index_assign idx n vptr lptr (fromIntegral k)
      if ret == 0
        then Right <$> peekIdxArray (fromIntegral n * k) lptr
        else return $ Left "Assignment failed"

-- | removes all elements from the database.
indexReset ::
  FaissIndex ->
  -- | opaque pointer to index object
  IO (Either String ())
indexReset idx = do
  ret <- c_faiss_Index_reset idx
  return $ if ret == 0 then Right () else Left "Reset failed"

-- | removes IDs from the index. Not supported by all indexes
indexRemoveIds ::
  FaissIndex ->
  -- | opaque pointer to index object
  FaissIDSelector ->
  IO (Either String Int)
indexRemoveIds idx sel = do
  alloca $ \ptr -> do
    ret <- c_faiss_Index_remove_ids idx sel ptr
    if ret == 0
      then Right . fromIntegral <$> peek ptr
      else return $ Left "Remove IDs failed"

{- |  Reconstruct a stored vector (or an approximation if lossy coding)
|  this function may not be defined for some indexes
-}
indexReconstruct ::
  FaissIndex ->
  -- | opaque pointer to index object
  Int ->
  -- | id of the vector to reconstruct
  Int ->
  -- | size of result vector
  IO (Either String [Float])
indexReconstruct idx key d = do
  allocaArray d $ \ptr -> do
    ret <- c_faiss_Index_reconstruct idx (fromIntegral key) ptr
    if ret == 0
      then Right <$> peekFloatArray d ptr
      else return $ Left "Reconstruction failed"

{- |  Reconstruct vectors i0 to i0 + ni - 1
| this function may not be defined for some indexes
-}
indexReconstructN ::
  FaissIndex ->
  -- | opaque pointer to index object
  Int ->
  -- | start index
  Int ->
  -- | till next n elements
  Int ->
  -- | length of the vector
  IO (Either String [Float])
indexReconstructN idx i0 ni d = do
  allocaArray (ni * d) $ \ptr -> do
    ret <- c_faiss_Index_reconstruct_n idx (fromIntegral i0) (fromIntegral ni) ptr
    if ret == 0
      then Right <$> peekFloatArray (ni * d) ptr
      else return $ Left "Reconstruction failed"

{- | Computes a residual vector after indexing encoding.
 The residual vector is the difference between a vector and the
 reconstruction that can be decoded from its representation in
 the index. The residual can be used for multiple-stage indexing
 methods, like IndexIVF's methods.
-}
indexComputeResidual ::
  FaissIndex ->
  -- | opaque pointer to index object
  [Float] ->
  -- | input vector, size d
  Int ->
  -- | encoded index, as returned by search and assign
  IO (Either String [Float])
indexComputeResidual idx vector key = do
  withFloatArray vector $ \vptr ->
    allocaArray (length vector) $ \rptr -> do
      ret <- c_faiss_Index_compute_residual idx vptr rptr (fromIntegral key)
      if ret == 0
        then Right <$> peekFloatArray (length vector) rptr
        else return $ Left "Compute residual failed"

{- | Compute residual vectors for multiple inputs
The residual vector is the difference between a vector and the
* reconstruction that can be decoded from its representation in
* the index. The residual can be used for multiple-stage indexing
* methods, like IndexIVF's methods.
-}
indexComputeResidualN ::
  FaissIndex ->
  -- | opaque pointer to index object
  [[Float]] ->
  [Int] ->
  IO (Either String [[Float]])
indexComputeResidualN idx vectors keys = do
  let n = length vectors
      normalized = concat vectors
      d = (length normalized) `div` n
  withFloatArray normalized $ \vptr ->
    withIdxArray keys $ \kptr ->
      allocaArray (n * d) $ \rptr -> do
        ret <- c_faiss_Index_compute_residual_n idx (fromIntegral n) vptr rptr kptr
        if ret == 0
          then Right <$> (toMatrix n d) <$> peekFloatArray (n * d) rptr
          else return $ Left "Compute residual failed"
  where
    toMatrix :: Int -> Int -> [Float] -> [[Float]]
    toMatrix n d xs
      | length xs /= n * d = error "Input list length must be n * d"
      | otherwise = go xs
      where
        go [] = []
        go ys = let (row, rest) = splitAt d ys in row : go rest

-- |  The size of the produced codes in bytes.
indexSaCodeSize :: FaissIndex -> IO (Either String Int)
indexSaCodeSize idx = do
  alloca $ \ptr -> do
    ret <- c_faiss_Index_sa_code_size idx ptr
    if ret == 0
      then Right . fromIntegral <$> peek ptr
      else return $ Left "Get code size failed"

-- |  encode a set of vectors
indexSaEncode ::
  FaissIndex ->
  -- | opaque pointer to index object
  [[Float]] ->
  -- | input vectors, size n * d
  IO (Either String [Word8])
indexSaEncode idx vectors = do
  let n = fromIntegral $ length vectors
      normalized = concat vectors
  codeSize <- indexSaCodeSize idx
  case codeSize of
    Left err -> return $ Left err
    Right cs -> do
      withFloatArray normalized $ \vptr ->
        allocaArray (fromIntegral n * cs) $ \bptr -> do
          ret <- c_faiss_Index_sa_encode idx n vptr bptr
          if ret == 0
            then Right <$> peekArray (fromIntegral n * cs) bptr
            else return $ Left "Encoding failed"

-- | decode a set of vectors
indexSaDecode ::
  FaissIndex ->
  -- | opaque pointer to index object
  [Word8] ->
  -- | input encoded vectors, size n * sa_code_size()
  Int ->
  -- | n
  Int ->
  -- | d
  IO (Either String [Float])
indexSaDecode idx bytes n d = do
  withArray bytes $ \bptr ->
    allocaArray (n * d) $ \vptr -> do
      ret <- c_faiss_Index_sa_decode idx (fromIntegral n) bptr vptr
      if ret == 0
        then Right <$> peekFloatArray (n * d) vptr
        else return $ Left "Decoding failed"

-- | Utility functions

-- | Marshal a list of Floats to a C array
withFloatArray :: [Float] -> (Ptr CFloat -> IO a) -> IO a
withFloatArray xs f = withArray (map realToFrac xs) f

-- | Marshal a list of Ints to a C array of IdxT
withIdxArray :: [Int] -> (Ptr IdxT -> IO a) -> IO a
withIdxArray xs f = withArray (map fromIntegral xs) f

-- | Peek a C array of CFloats and convert to [Float]
peekFloatArray :: Int -> Ptr CFloat -> IO [Float]
peekFloatArray n ptr = map realToFrac <$> peekArray n ptr

-- | Peek a C array of IdxT and convert to [Int]
peekIdxArray :: Int -> Ptr IdxT -> IO [Int]
peekIdxArray n ptr = map fromIntegral <$> peekArray n ptr
