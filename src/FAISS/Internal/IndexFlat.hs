{-# LANGUAGE ForeignFunctionInterface #-}

module FAISS.Internal.IndexFlat
  ( -- * Types
    FaissIndexFlat
  , FaissIndexFlatIP
  , FaissIndexFlatL2
  , FaissIndexFlat1D
  , FaissIndexRefineFlat

    -- * IndexFlat Constructors
  , c_faiss_IndexFlat_new
  , c_faiss_IndexFlat_new_with

    -- * Access Internal Data
  , c_faiss_IndexFlat_xb

    -- * Distance Computation
  , c_faiss_IndexFlat_compute_distance_subset

    -- * Downcasting Functions
  , c_faiss_IndexFlat_cast
  , c_faiss_IndexFlatIP_cast
  , c_faiss_IndexFlatL2_cast
  , c_faiss_IndexFlat1D_cast
  , c_faiss_IndexRefineFlat_cast

    -- * Destructors
  , c_faiss_IndexFlat_free
  , c_faiss_IndexFlatIP_free
  , c_faiss_IndexFlatL2_free
  , c_faiss_IndexFlat1D_free
  , c_faiss_IndexRefineFlat_free

    -- * IndexFlatIP Functions
  , c_faiss_IndexFlatIP_new
  , c_faiss_IndexFlatIP_new_with

    -- * IndexFlatL2 Functions
  , c_faiss_IndexFlatL2_new
  , c_faiss_IndexFlatL2_new_with

    -- * IndexRefineFlat Functions
  , c_faiss_IndexRefineFlat_new
  , c_faiss_IndexRefineFlat_own_fields
  , c_faiss_IndexRefineFlat_set_own_fields
  , c_faiss_IndexRefineFlat_k_factor
  , c_faiss_IndexRefineFlat_set_k_factor

    -- * IndexFlat1D Functions
  , c_faiss_IndexFlat1D_new
  , c_faiss_IndexFlat1D_new_with
  , c_faiss_IndexFlat1D_update_permutation
  ) where

import FAISS.Internal.Index
import Foreign
import Foreign.C.Types

-- Opaque pointer types
data FaissIndexFlat_
data FaissIndexFlatIP_
data FaissIndexFlatL2_
data FaissIndexFlat1D_
data FaissIndexRefineFlat_

type FaissIndexFlat = Ptr FaissIndexFlat_
type FaissIndexFlatIP = Ptr FaissIndexFlatIP_
type FaissIndexFlatL2 = Ptr FaissIndexFlatL2_
type FaissIndexFlat1D = Ptr FaissIndexFlat1D_
type FaissIndexRefineFlat = Ptr FaissIndexRefineFlat_

-- Constructors
foreign import ccall unsafe "faiss_IndexFlat_new"
  c_faiss_IndexFlat_new :: Ptr FaissIndexFlat -> IO CInt

foreign import ccall unsafe "faiss_IndexFlat_new_with"
  c_faiss_IndexFlat_new_with :: Ptr FaissIndexFlat -> IdxT -> CInt -> IO CInt

-- Access internal data
foreign import ccall unsafe "faiss_IndexFlat_xb"
  c_faiss_IndexFlat_xb :: FaissIndexFlat -> Ptr (Ptr CFloat) -> Ptr CSize -> IO ()

-- Distance computation with subset
foreign import ccall unsafe "faiss_IndexFlat_compute_distance_subset"
  c_faiss_IndexFlat_compute_distance_subset ::
    FaissIndex ->
    IdxT ->
    Ptr CFloat ->
    IdxT ->
    Ptr CFloat ->
    Ptr IdxT ->
    IO CInt

-- Downcasting
foreign import ccall unsafe "faiss_IndexFlat_cast"
  c_faiss_IndexFlat_cast :: FaissIndex -> IO FaissIndexFlat

foreign import ccall unsafe "faiss_IndexFlatIP_cast"
  c_faiss_IndexFlatIP_cast :: FaissIndex -> IO FaissIndexFlatIP

foreign import ccall unsafe "faiss_IndexFlatL2_cast"
  c_faiss_IndexFlatL2_cast :: FaissIndex -> IO FaissIndexFlatL2

foreign import ccall unsafe "faiss_IndexFlat1D_cast"
  c_faiss_IndexFlat1D_cast :: FaissIndex -> IO FaissIndexFlat1D

foreign import ccall unsafe "faiss_IndexRefineFlat_cast"
  c_faiss_IndexRefineFlat_cast :: FaissIndex -> IO FaissIndexRefineFlat

-- Destructors
foreign import ccall unsafe "&faiss_IndexFlat_free"
  c_faiss_IndexFlat_free :: FunPtr (FaissIndexFlat -> IO ())

foreign import ccall unsafe "&faiss_IndexFlatIP_free"
  c_faiss_IndexFlatIP_free :: FunPtr (FaissIndexFlatIP -> IO ())

foreign import ccall unsafe "&faiss_IndexFlatL2_free"
  c_faiss_IndexFlatL2_free :: FunPtr (FaissIndexFlatL2 -> IO ())

foreign import ccall unsafe "&faiss_IndexFlat1D_free"
  c_faiss_IndexFlat1D_free :: FunPtr (FaissIndexFlat1D -> IO ())

foreign import ccall unsafe "&faiss_IndexRefineFlat_free"
  c_faiss_IndexRefineFlat_free :: FunPtr (FaissIndexRefineFlat -> IO ())

-- IndexFlatIP
foreign import ccall unsafe "faiss_IndexFlatIP_new"
  c_faiss_IndexFlatIP_new :: Ptr FaissIndexFlatIP -> IO CInt

foreign import ccall unsafe "faiss_IndexFlatIP_new_with"
  c_faiss_IndexFlatIP_new_with :: Ptr FaissIndexFlatIP -> IdxT -> IO CInt

-- IndexFlatL2
foreign import ccall unsafe "faiss_IndexFlatL2_new"
  c_faiss_IndexFlatL2_new :: Ptr FaissIndexFlatL2 -> IO CInt

foreign import ccall unsafe "faiss_IndexFlatL2_new_with"
  c_faiss_IndexFlatL2_new_with :: Ptr FaissIndexFlatL2 -> IdxT -> IO CInt

-- IndexRefineFlat
foreign import ccall unsafe "faiss_IndexRefineFlat_new"
  c_faiss_IndexRefineFlat_new :: Ptr FaissIndexRefineFlat -> FaissIndex -> IO CInt

foreign import ccall unsafe "faiss_IndexRefineFlat_own_fields"
  c_faiss_IndexRefineFlat_own_fields :: FaissIndexRefineFlat -> IO CInt

foreign import ccall unsafe "faiss_IndexRefineFlat_set_own_fields"
  c_faiss_IndexRefineFlat_set_own_fields :: FaissIndexRefineFlat -> CInt -> IO ()

foreign import ccall unsafe "faiss_IndexRefineFlat_k_factor"
  c_faiss_IndexRefineFlat_k_factor :: FaissIndexRefineFlat -> IO CFloat

foreign import ccall unsafe "faiss_IndexRefineFlat_set_k_factor"
  c_faiss_IndexRefineFlat_set_k_factor :: FaissIndexRefineFlat -> CFloat -> IO ()

-- IndexFlat1D
foreign import ccall unsafe "faiss_IndexFlat1D_new"
  c_faiss_IndexFlat1D_new :: Ptr FaissIndexFlat1D -> IO CInt

foreign import ccall unsafe "faiss_IndexFlat1D_new_with"
  c_faiss_IndexFlat1D_new_with :: Ptr FaissIndexFlat1D -> CInt -> IO CInt

foreign import ccall unsafe "faiss_IndexFlat1D_update_permutation"
  c_faiss_IndexFlat1D_update_permutation :: FaissIndexFlat1D -> IO CInt
