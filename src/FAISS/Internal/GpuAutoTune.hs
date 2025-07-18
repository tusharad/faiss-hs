{-# LANGUAGE ForeignFunctionInterface #-}

module FAISS.Internal.GpuAutoTune (
    -- CPU <-> GPU conversion
    c_faiss_index_gpu_to_cpu,
    c_faiss_index_cpu_to_gpu,
    c_faiss_index_cpu_to_gpu_with_options,
    c_faiss_index_cpu_to_gpu_multiple,
    c_faiss_index_cpu_to_gpu_multiple_with_options
) where

import Foreign
import Foreign.C.Types
import FAISS.Internal.Index
import FAISS.Internal.GpuClonerOptions
import FAISS.Internal.GpuResources

-- CPU <-> GPU conversion

foreign import ccall unsafe "faiss_index_gpu_to_cpu"
  c_faiss_index_gpu_to_cpu :: FaissIndex -> Ptr FaissIndex -> IO CInt

foreign import ccall unsafe "faiss_index_cpu_to_gpu"
  c_faiss_index_cpu_to_gpu ::
    FaissGpuResourcesProvider -> CInt -> FaissIndex -> Ptr FaissIndex -> IO CInt

foreign import ccall unsafe "faiss_index_cpu_to_gpu_with_options"
  c_faiss_index_cpu_to_gpu_with_options ::
    FaissGpuResourcesProvider ->
    CInt ->
    FaissIndex ->
    FaissGpuClonerOptions ->
    Ptr FaissIndex ->
    IO CInt

foreign import ccall unsafe "faiss_index_cpu_to_gpu_multiple"
  c_faiss_index_cpu_to_gpu_multiple ::
    Ptr FaissGpuResourcesProvider ->
    Ptr CInt ->
    CSize ->
    FaissIndex ->
    Ptr FaissIndex ->
    IO CInt

foreign import ccall unsafe "faiss_index_cpu_to_gpu_multiple_with_options"
  c_faiss_index_cpu_to_gpu_multiple_with_options ::
    Ptr FaissGpuResourcesProvider ->
    CSize ->
    Ptr CInt ->
    CSize ->
    FaissIndex ->
    FaissGpuMultipleClonerOptions ->
    Ptr FaissIndex ->
    IO CInt
