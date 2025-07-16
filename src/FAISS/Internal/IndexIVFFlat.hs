module FAISS.Internal.IndexIVFFlat where

import FAISS.Internal.Index
import Foreign
import Foreign.C.Types

-- Opaque pointer type
data FaissIndexIVFFlat_
type FaissIndexIVFFlat = Ptr FaissIndexIVFFlat_

-- Constructors
foreign import ccall unsafe "faiss_IndexIVFFlat_new"
  c_faiss_IndexIVFFlat_new :: Ptr FaissIndexIVFFlat -> IO CInt

foreign import ccall unsafe "faiss_IndexIVFFlat_new_with"
  c_faiss_IndexIVFFlat_new_with ::
    Ptr FaissIndexIVFFlat ->
    FaissIndex ->
    CSize ->
    CSize ->
    IO CInt

foreign import ccall unsafe "faiss_IndexIVFFlat_new_with_metric"
  c_faiss_IndexIVFFlat_new_with_metric ::
    Ptr FaissIndexIVFFlat -> FaissIndex -> CSize -> CSize -> CInt -> IO CInt

-- Core Add
foreign import ccall unsafe "faiss_IndexIVFFlat_add_core"
  c_faiss_IndexIVFFlat_add_core ::
    FaissIndexIVFFlat -> IdxT -> Ptr CFloat -> Ptr IdxT -> Ptr Int64 -> IO CInt

-- Update
foreign import ccall unsafe "faiss_IndexIVFFlat_update_vectors"
  c_faiss_IndexIVFFlat_update_vectors ::
    FaissIndexIVFFlat -> CInt -> Ptr IdxT -> Ptr CFloat -> IO CInt

foreign import ccall unsafe "faiss_IndexIVFFlat_set_nprobe"
  c_faiss_IndexIVFFlat_set_nprobe :: FaissIndexIVFFlat -> CSize -> IO ()
