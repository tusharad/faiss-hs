{- |
Module      : FAISS.IndexFactory
Description : Module containing operations related to IndexFactory
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

High-level Haskell interface functions for IndexFactory
-}
module FAISS.IndexFactory
  ( indexFactory
  ) where

import FAISS.Internal.Index
import FAISS.Internal.IndexFactory
import Foreign
import Foreign.C.String

-- | Build an index with the sequence of processing steps described in the string.
indexFactory :: Int -> String -> FaissMetricType -> IO (Either String FaissIndex)
indexFactory d description metric
  | d < 1 = pure $ Left "dimension should be greater than 0"
  | otherwise =
      withCString description $ \descCStr ->
        alloca $ \ptrPtr -> do
          ret <- c_faiss_index_factory ptrPtr (fromIntegral d) descCStr (fromIntegral $ fromEnum metric)
          if ret == 0
            then do
              idxPtr <- peek ptrPtr
              return $ Right idxPtr
            else return $ Left "faiss_index_factory failed"
