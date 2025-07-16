{-# LANGUAGE ForeignFunctionInterface #-}

module FAISS.Internal.GpuClonerOptions where

import Foreign
import Foreign.C.Types

-- Opaque pointer types
data FaissGpuClonerOptions_
type FaissGpuClonerOptions = Ptr FaissGpuClonerOptions_

data FaissGpuMultipleClonerOptions_
type FaissGpuMultipleClonerOptions = Ptr FaissGpuMultipleClonerOptions_

data FaissGpuIndexConfig_
type FaissGpuIndexConfig = Ptr FaissGpuIndexConfig_

-- Enum for FaissIndicesOptions
data FaissIndicesOptions
  = IndicesCPU
  | IndicesIVF
  | Indices32Bit
  | Indices64Bit
  deriving (Show, Eq, Enum)

-- Constructor
foreign import ccall unsafe "faiss_GpuClonerOptions_new"
  c_faiss_GpuClonerOptions_new :: Ptr FaissGpuClonerOptions -> IO CInt

foreign import ccall unsafe "faiss_GpuMultipleClonerOptions_new"
  c_faiss_GpuMultipleClonerOptions_new :: Ptr FaissGpuMultipleClonerOptions -> IO CInt

foreign import ccall unsafe "&faiss_GpuClonerOptions_free"
  c_faiss_GpuClonerOptions_free :: FunPtr (FaissGpuClonerOptions -> IO ())

-- Getters / setters
foreign import ccall unsafe "faiss_GpuClonerOptions_set_indicesOptions"
  c_set_indicesOptions :: FaissGpuClonerOptions -> CInt -> IO ()

foreign import ccall unsafe "faiss_GpuClonerOptions_get_indicesOptions"
  c_get_indicesOptions :: FaissGpuClonerOptions -> IO CInt

foreign import ccall unsafe "faiss_GpuClonerOptions_set_useFloat16CoarseQuantizer"
  c_set_useFloat16CoarseQuantizer :: FaissGpuClonerOptions -> CInt -> IO ()

foreign import ccall unsafe "faiss_GpuClonerOptions_get_useFloat16CoarseQuantizer"
  c_get_useFloat16CoarseQuantizer :: FaissGpuClonerOptions -> IO CInt

foreign import ccall unsafe "faiss_GpuClonerOptions_set_useFloat16"
  c_set_useFloat16 :: FaissGpuClonerOptions -> CInt -> IO ()

foreign import ccall unsafe "faiss_GpuClonerOptions_get_useFloat16"
  c_get_useFloat16 :: FaissGpuClonerOptions -> IO CInt

foreign import ccall unsafe "faiss_GpuClonerOptions_set_usePrecomputed"
  c_set_usePrecomputed :: FaissGpuClonerOptions -> CInt -> IO ()

foreign import ccall unsafe "faiss_GpuClonerOptions_get_usePrecomputed"
  c_get_usePrecomputed :: FaissGpuClonerOptions -> IO CInt

foreign import ccall unsafe "faiss_GpuClonerOptions_set_reserveVecs"
  c_set_reserveVecs :: FaissGpuClonerOptions -> CLong -> IO ()

foreign import ccall unsafe "faiss_GpuClonerOptions_get_reserveVecs"
  c_get_reserveVecs :: FaissGpuClonerOptions -> IO CLong

foreign import ccall unsafe "faiss_GpuClonerOptions_set_storeTransposed"
  c_set_storeTransposed :: FaissGpuClonerOptions -> CInt -> IO ()

foreign import ccall unsafe "faiss_GpuClonerOptions_get_storeTransposed"
  c_get_storeTransposed :: FaissGpuClonerOptions -> IO CInt

foreign import ccall unsafe "faiss_GpuClonerOptions_set_verbose"
  c_set_verbose :: FaissGpuClonerOptions -> CInt -> IO ()

foreign import ccall unsafe "faiss_GpuClonerOptions_get_verbose"
  c_get_verbose :: FaissGpuClonerOptions -> IO CInt

-- Multiple cloner specific
foreign import ccall unsafe "faiss_GpuMultipleClonerOptions_set_shard"
  c_set_shard :: FaissGpuMultipleClonerOptions -> CInt -> IO ()

foreign import ccall unsafe "faiss_GpuMultipleClonerOptions_get_shard"
  c_get_shard :: FaissGpuMultipleClonerOptions -> IO CInt

foreign import ccall unsafe "faiss_GpuMultipleClonerOptions_set_shard_type"
  c_set_shard_type :: FaissGpuMultipleClonerOptions -> CInt -> IO ()

foreign import ccall unsafe "faiss_GpuMultipleClonerOptions_get_shard_type"
  c_get_shard_type :: FaissGpuMultipleClonerOptions -> IO CInt

-- GpuIndexConfig
foreign import ccall unsafe "faiss_GpuIndexConfig_get_device"
  c_get_device :: FaissGpuIndexConfig -> IO CInt
