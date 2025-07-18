{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : FAISS.Internal.Index
Description : FFIs for Index
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Foreign imports for FAISS C functions for Index module.
 https://github.com/facebookresearch/faiss/blob/main/c_api/Index_c.h
-}
module FAISS.Internal.Index
  ( FaissIDSelector
  , FaissSearchParameters
  , FaissIndex
  , FaissMetricType (..)
  , FaissRangeSearchResult
  , IdxT
  , c_faiss_SearchParameters_new
  , c_faiss_IDSelectorRange_free
  , c_faiss_SearchParameters_free
  , c_faiss_Index_free
  , c_faiss_Index_d
  , c_faiss_Index_is_trained
  , c_faiss_Index_ntotal
  , c_faiss_Index_metric_type
  , c_faiss_Index_verbose
  , c_faiss_Index_set_verbose
  , c_faiss_Index_train
  , c_faiss_Index_add
  , c_faiss_Index_add_with_ids
  , c_faiss_Index_search
  , c_faiss_Index_search_with_params
  , c_faiss_Index_range_search
  , c_faiss_Index_assign
  , c_faiss_Index_reset
  , c_faiss_Index_remove_ids
  , c_faiss_Index_reconstruct
  , c_faiss_Index_reconstruct_n
  , c_faiss_Index_compute_residual
  , c_faiss_Index_compute_residual_n
  , c_faiss_Index_sa_code_size
  , c_faiss_Index_sa_encode
  , c_faiss_Index_sa_decode
  , c_faiss_write_index_fname
  , c_faiss_IDSelector_free
  ) where

import Foreign
import Foreign.C.Types
import GHC.Generics

-- | Type alias for index type used in FAISS
type IdxT = CLong

-- | Opaque pointer to FaissIndex
data FaissIndex_

-- | Opaque pointer to FaissSearchParameters
data FaissSearchParameters_

-- | Opaque pointer to FaissRangeSearchResult
data FaissRangeSearchResult_

-- | Opaque pointer to FaissIDSelector
data FaissIDSelector_

-- | Type aliases for the opaque pointers
type FaissIndex = Ptr FaissIndex_

type FaissSearchParameters = Ptr FaissSearchParameters_
type FaissIDSelector = Ptr FaissIDSelector_

type FaissRangeSearchResult = Ptr FaissRangeSearchResult_

-- | Metric types supported by FAISS
data FaissMetricType
  = -- | Maximum inner product search
    MetricInnerProduct
  | -- | Squared L2 search
    MetricL2
  | -- | L1 (aka cityblock)
    MetricL1
  | -- | Infinity distance
    MetricLinf
  | -- | L_p distance, p is given by metric_arg
    MetricLp
  | -- | Canberra distance
    MetricCanberra
  | -- | Bray-Curtis distance
    MetricBrayCurtis
  | -- | Jensen-Shannon distance
    MetricJensenShannon
  deriving (Show, Eq, Generic)

instance Enum FaissMetricType where
  fromEnum MetricInnerProduct = 0
  fromEnum MetricL2 = 1
  fromEnum MetricL1 = 2
  fromEnum MetricLinf = 3
  fromEnum MetricLp = 4
  fromEnum MetricCanberra = 20
  fromEnum MetricBrayCurtis = 21
  fromEnum MetricJensenShannon = 22

  toEnum 0 = MetricInnerProduct
  toEnum 1 = MetricL2
  toEnum 2 = MetricL1
  toEnum 3 = MetricLinf
  toEnum 4 = MetricLp
  toEnum 20 = MetricCanberra
  toEnum 21 = MetricBrayCurtis
  toEnum 22 = MetricJensenShannon
  toEnum n = error $ "Unknown FaissMetricType: " ++ show n

-- Search parameters
foreign import ccall unsafe "faiss_SearchParameters_new"
  c_faiss_SearchParameters_new :: Ptr FaissSearchParameters -> FaissIDSelector -> IO CInt

foreign import ccall unsafe "faiss_SearchParameters_free"
  c_faiss_SearchParameters_free :: FaissSearchParameters -> IO ()

foreign import ccall unsafe "faiss_IDSelectorRange_free"
  c_faiss_IDSelectorRange_free :: FaissIDSelector -> IO ()

foreign import ccall unsafe "faiss_IDSelector_free"
  c_faiss_IDSelector_free :: FaissIDSelector -> IO ()

foreign import ccall unsafe "faiss_Index_free"
  c_faiss_Index_free :: FaissIndex -> IO ()

-- Index getters
foreign import ccall unsafe "faiss_Index_d"
  c_faiss_Index_d :: FaissIndex -> IO CInt

foreign import ccall unsafe "faiss_Index_is_trained"
  c_faiss_Index_is_trained :: FaissIndex -> IO CInt

foreign import ccall unsafe "faiss_Index_ntotal"
  c_faiss_Index_ntotal :: FaissIndex -> IO IdxT

foreign import ccall unsafe "faiss_Index_metric_type"
  c_faiss_Index_metric_type :: FaissIndex -> IO CInt

foreign import ccall unsafe "faiss_Index_verbose"
  c_faiss_Index_verbose :: FaissIndex -> IO CInt

-- Index setters
foreign import ccall unsafe "faiss_Index_set_verbose"
  c_faiss_Index_set_verbose :: FaissIndex -> CInt -> IO ()

-- Index operations
foreign import ccall unsafe "faiss_Index_train"
  c_faiss_Index_train :: FaissIndex -> IdxT -> Ptr CFloat -> IO CInt

foreign import ccall unsafe "faiss_Index_add"
  c_faiss_Index_add :: FaissIndex -> IdxT -> Ptr CFloat -> IO CInt

foreign import ccall unsafe "faiss_Index_add_with_ids"
  c_faiss_Index_add_with_ids :: FaissIndex -> IdxT -> Ptr CFloat -> Ptr IdxT -> IO CInt

foreign import ccall unsafe "faiss_Index_search"
  c_faiss_Index_search ::
    FaissIndex -> IdxT -> Ptr CFloat -> IdxT -> Ptr CFloat -> Ptr IdxT -> IO CInt

foreign import ccall unsafe "faiss_Index_search_with_params"
  c_faiss_Index_search_with_params ::
    FaissIndex ->
    IdxT ->
    Ptr CFloat ->
    IdxT ->
    FaissSearchParameters ->
    Ptr CFloat ->
    Ptr IdxT ->
    IO CInt

foreign import ccall unsafe "faiss_Index_range_search"
  c_faiss_Index_range_search ::
    FaissIndex -> IdxT -> Ptr CFloat -> CFloat -> FaissRangeSearchResult -> IO CInt

foreign import ccall unsafe "faiss_Index_assign"
  c_faiss_Index_assign :: FaissIndex -> IdxT -> Ptr CFloat -> Ptr IdxT -> IdxT -> IO CInt

foreign import ccall unsafe "faiss_Index_reset"
  c_faiss_Index_reset :: FaissIndex -> IO CInt

foreign import ccall unsafe "faiss_Index_remove_ids"
  c_faiss_Index_remove_ids :: FaissIndex -> FaissIDSelector -> Ptr CSize -> IO CInt

foreign import ccall unsafe "faiss_Index_reconstruct"
  c_faiss_Index_reconstruct :: FaissIndex -> IdxT -> Ptr CFloat -> IO CInt

foreign import ccall unsafe "faiss_Index_reconstruct_n"
  c_faiss_Index_reconstruct_n :: FaissIndex -> IdxT -> IdxT -> Ptr CFloat -> IO CInt

foreign import ccall unsafe "faiss_Index_compute_residual"
  c_faiss_Index_compute_residual :: FaissIndex -> Ptr CFloat -> Ptr CFloat -> IdxT -> IO CInt

foreign import ccall unsafe "faiss_Index_compute_residual_n"
  c_faiss_Index_compute_residual_n ::
    FaissIndex -> IdxT -> Ptr CFloat -> Ptr CFloat -> Ptr IdxT -> IO CInt

-- Standalone codec interface
foreign import ccall unsafe "faiss_Index_sa_code_size"
  c_faiss_Index_sa_code_size :: FaissIndex -> Ptr CSize -> IO CInt

foreign import ccall unsafe "faiss_Index_sa_encode"
  c_faiss_Index_sa_encode :: FaissIndex -> IdxT -> Ptr CFloat -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "faiss_Index_sa_decode"
  c_faiss_Index_sa_decode :: FaissIndex -> IdxT -> Ptr Word8 -> Ptr CFloat -> IO CInt

foreign import ccall unsafe "faiss_write_index_fname"
  c_faiss_write_index_fname :: FaissIndex -> Ptr CChar -> IO CInt
