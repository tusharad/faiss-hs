{- |
Module      : FAISS.Internal.IndexFactory
Description : FFIs for IndexFactory
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

 https://github.com/facebookresearch/faiss/blob/main/c_api/Index_c.h
-}
module FAISS.Internal.IndexFactory
  ( c_faiss_index_factory
  ) where

import FAISS.Internal.Index
import Foreign
import Foreign.C.String
import Foreign.C.Types

-- | faiss_index_factory: creates a standard index
foreign import ccall unsafe "faiss_index_factory"
  c_faiss_index_factory :: Ptr FaissIndex -> CInt -> CString -> CInt -> IO CInt
