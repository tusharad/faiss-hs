{- |
Module      : FAISS.AuxIndexStructures
Description : Bindings for RangeSearchResult and IDSelector utilities
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Bindings for auxiliary FAISS index structures like RangeSearchResult and IDSelector.
-}
module FAISS.AuxIndexStructures
  ( -- * RangeSearchResult
    FaissRangeSearchResult
  , rangeSearchResultNew
  , rangeSearchResultNewWith
  , rangeSearchResultDoAllocation
  , getRangeSearchResultNQ
  , getRangeSearchResultBufferSize
  , getRangeSearchResultLims
  , getRangeSearchResultLabels

    -- * IDSelector
  , FaissIDSelector
  , idSelectorIsMember
  , idSelectorRangeNew
  , idSelectorNotNew
  , idSelectorAndNew
  , idSelectorOrNew
  , idSelectorXOrNew
  ) where

import Foreign
import Foreign.C.Types
import FAISS.Internal.Index
import Control.Monad (liftM2)

-- Foreign imports
foreign import ccall unsafe "faiss_RangeSearchResult_new"
  c_faiss_RangeSearchResult_new :: Ptr FaissRangeSearchResult -> IdxT -> IO CInt

foreign import ccall unsafe "faiss_RangeSearchResult_new_with"
  c_faiss_RangeSearchResult_new_with :: 
    Ptr FaissRangeSearchResult -> IdxT -> CInt -> IO CInt

foreign import ccall unsafe "faiss_RangeSearchResult_do_allocation"
  c_faiss_RangeSearchResult_do_allocation :: FaissRangeSearchResult -> IO CInt

foreign import ccall unsafe "faiss_RangeSearchResult_nq"
  c_faiss_RangeSearchResult_nq :: FaissRangeSearchResult -> IO CSize

foreign import ccall unsafe "faiss_RangeSearchResult_buffer_size"
  c_faiss_RangeSearchResult_buffer_size :: FaissRangeSearchResult -> IO CSize

foreign import ccall unsafe "faiss_RangeSearchResult_lims"
  c_faiss_RangeSearchResult_lims :: FaissRangeSearchResult -> Ptr (Ptr CSize) -> IO ()

foreign import ccall unsafe "faiss_RangeSearchResult_labels"
  c_faiss_RangeSearchResult_labels :: 
    FaissRangeSearchResult -> Ptr (Ptr IdxT) -> Ptr (Ptr CFloat) -> IO ()

-- | Create new RangeSearchResult
rangeSearchResultNew :: Int -> IO (Either String FaissRangeSearchResult)
rangeSearchResultNew nq = alloca $ \ptr -> do
  ret <- c_faiss_RangeSearchResult_new ptr (fromIntegral nq)
  if ret == 0 then Right <$> peek ptr 
  else return $ Left "Failed to create RangeSearchResult"

-- | Create new RangeSearchResult with allocation flag
rangeSearchResultNewWith :: Int -> Bool -> IO (Either String FaissRangeSearchResult)
rangeSearchResultNewWith nq allocLims = alloca $ \ptr -> do
  let allocFlag = if allocLims then 1 else 0
  ret <- c_faiss_RangeSearchResult_new_with ptr (fromIntegral nq) allocFlag
  if ret == 0 then Right <$> peek ptr 
  else return $ Left "Failed to create RangeSearchResult (with alloc)"

-- | Trigger allocation for RangeSearchResult
rangeSearchResultDoAllocation :: FaissRangeSearchResult -> IO (Either String ())
rangeSearchResultDoAllocation rsr = do
  ret <- c_faiss_RangeSearchResult_do_allocation rsr
  pure $ if ret == 0 then Right () else Left "Failed to allocate RangeSearchResult"

-- | Get number of queries
getRangeSearchResultNQ :: FaissRangeSearchResult -> IO Int
getRangeSearchResultNQ rsr = fromIntegral <$> c_faiss_RangeSearchResult_nq rsr

-- | Get buffer size
getRangeSearchResultBufferSize :: FaissRangeSearchResult -> IO Int
getRangeSearchResultBufferSize rsr = 
    fromIntegral <$> c_faiss_RangeSearchResult_buffer_size rsr

-- | Get lims array (length = nq + 1)
getRangeSearchResultLims :: FaissRangeSearchResult -> IO (Ptr CSize)
getRangeSearchResultLims rsr = alloca $ \ptrPtr -> 
    c_faiss_RangeSearchResult_lims rsr ptrPtr >> peek ptrPtr

-- | Get labels and distances
getRangeSearchResultLabels :: FaissRangeSearchResult -> IO (Ptr IdxT, Ptr CFloat)
getRangeSearchResultLabels rsr = alloca $ \lPtr -> alloca $ \dPtr -> do
  c_faiss_RangeSearchResult_labels rsr lPtr dPtr
  liftM2 (,) (peek lPtr) (peek dPtr)

-- IDSelector related FFI
foreign import ccall unsafe "faiss_IDSelector_is_member"
  c_faiss_IDSelector_is_member :: FaissIDSelector -> IdxT -> IO CInt

foreign import ccall unsafe "faiss_IDSelectorRange_new"
  c_faiss_IDSelectorRange_new :: Ptr FaissIDSelector -> IdxT -> IdxT -> IO CInt

foreign import ccall unsafe "faiss_IDSelectorNot_new"
  c_faiss_IDSelectorNot_new :: Ptr FaissIDSelector -> FaissIDSelector -> IO CInt

foreign import ccall unsafe "faiss_IDSelectorAnd_new"
  c_faiss_IDSelectorAnd_new :: Ptr FaissIDSelector -> 
    FaissIDSelector -> FaissIDSelector -> IO CInt

foreign import ccall unsafe "faiss_IDSelectorOr_new"
  c_faiss_IDSelectorOr_new :: Ptr FaissIDSelector -> 
    FaissIDSelector -> FaissIDSelector -> IO CInt

foreign import ccall unsafe "faiss_IDSelectorXOr_new"
  c_faiss_IDSelectorXOr_new :: Ptr FaissIDSelector -> 
    FaissIDSelector -> FaissIDSelector -> IO CInt

-- | Check if ID is in selector
idSelectorIsMember :: FaissIDSelector -> Int -> IO Bool
idSelectorIsMember sel id' = (/= 0) <$> c_faiss_IDSelector_is_member sel (fromIntegral id')

-- | Create an IDSelectorRange
idSelectorRangeNew :: Int -> Int -> IO (Either String FaissIDSelector)
idSelectorRangeNew imin imax = alloca $ \ptr -> do
  ret <- c_faiss_IDSelectorRange_new ptr (fromIntegral imin) (fromIntegral imax)
  if ret == 0 then Right <$> peek ptr else return $ Left "Failed to create IDSelectorRange"

-- | Create IDSelectorNot
idSelectorNotNew :: FaissIDSelector -> IO (Either String FaissIDSelector)
idSelectorNotNew sel = alloca $ \ptr -> do
  ret <- c_faiss_IDSelectorNot_new ptr sel
  if ret == 0 then Right <$> peek ptr else return $ Left "Failed to create IDSelectorNot"

-- | Create IDSelectorAnd
idSelectorAndNew :: 
    FaissIDSelector -> FaissIDSelector -> IO (Either String FaissIDSelector)
idSelectorAndNew lhs rhs = alloca $ \ptr -> do
  ret <- c_faiss_IDSelectorAnd_new ptr lhs rhs
  if ret == 0 then Right <$> peek ptr else return $ Left "Failed to create IDSelectorAnd"

-- | Create IDSelectorOr
idSelectorOrNew :: 
    FaissIDSelector -> FaissIDSelector -> IO (Either String FaissIDSelector)
idSelectorOrNew lhs rhs = alloca $ \ptr -> do
  ret <- c_faiss_IDSelectorOr_new ptr lhs rhs
  if ret == 0 then Right <$> peek ptr else return $ Left "Failed to create IDSelectorOr"

-- | Create IDSelectorXOr
idSelectorXOrNew :: 
    FaissIDSelector -> FaissIDSelector -> IO (Either String FaissIDSelector)
idSelectorXOrNew lhs rhs = alloca $ \ptr -> do
  ret <- c_faiss_IDSelectorXOr_new ptr lhs rhs
  if ret == 0 then Right <$> peek ptr else return $ Left "Failed to create IDSelectorXOr"
