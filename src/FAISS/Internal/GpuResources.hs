{-# LANGUAGE ForeignFunctionInterface #-}

module FAISS.Internal.GpuResources (
    -- Opaque Pointer Types
    FaissGpuResources,
    FaissGpuResourcesProvider,
    -- CUDA Type Aliases
    CublasHandle,
    CudaStream,
    -- Destructor
    c_faiss_GpuResources_free,
    -- Initialization
    c_faiss_GpuResources_initializeForDevice,
    -- Getters and Stream Management
    c_faiss_GpuResources_getBlasHandle,
    c_faiss_GpuResources_getDefaultStream,
    c_faiss_GpuResources_getAsyncCopyStream,
    c_faiss_GpuResources_getPinnedMemory,
    c_faiss_GpuResources_getBlasHandleCurrentDevice,
    c_faiss_GpuResources_getDefaultStreamCurrentDevice,
    c_faiss_GpuResources_syncDefaultStream,
    c_faiss_GpuResources_syncDefaultStreamCurrentDevice,
    c_faiss_GpuResources_getAsyncCopyStreamCurrentDevice,
    -- Provider API
    c_faiss_GpuResourcesProvider_getResources
) where

import Foreign
import Foreign.C.Types

-- | Opaque C pointers
data FaissGpuResources_
type FaissGpuResources = Ptr FaissGpuResources_

data FaissGpuResourcesProvider_
type FaissGpuResourcesProvider = Ptr FaissGpuResourcesProvider_

type CublasHandle = Ptr ()
type CudaStream = Ptr ()

foreign import ccall unsafe "&faiss_GpuResources_free"
  c_faiss_GpuResources_free :: FunPtr (FaissGpuResources -> IO ())

-- Initialization
foreign import ccall unsafe "faiss_GpuResources_initializeForDevice"
  c_faiss_GpuResources_initializeForDevice :: FaissGpuResources -> CInt -> IO CInt

-- Get handles and streams
foreign import ccall unsafe "faiss_GpuResources_getBlasHandle"
  c_faiss_GpuResources_getBlasHandle :: 
    FaissGpuResources -> CInt -> Ptr CublasHandle -> IO CInt

foreign import ccall unsafe "faiss_GpuResources_getDefaultStream"
  c_faiss_GpuResources_getDefaultStream :: 
    FaissGpuResources -> CInt -> Ptr CudaStream -> IO CInt

foreign import ccall unsafe "faiss_GpuResources_getAsyncCopyStream"
  c_faiss_GpuResources_getAsyncCopyStream :: 
    FaissGpuResources -> CInt -> Ptr CudaStream -> IO CInt

foreign import ccall unsafe "faiss_GpuResources_getPinnedMemory"
  c_faiss_GpuResources_getPinnedMemory :: 
    FaissGpuResources -> Ptr (Ptr ()) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "faiss_GpuResources_getBlasHandleCurrentDevice"
  c_faiss_GpuResources_getBlasHandleCurrentDevice :: 
    FaissGpuResources -> Ptr CublasHandle -> IO CInt

foreign import ccall unsafe "faiss_GpuResources_getDefaultStreamCurrentDevice"
  c_faiss_GpuResources_getDefaultStreamCurrentDevice :: 
    FaissGpuResources -> Ptr CudaStream -> IO CInt

foreign import ccall unsafe "faiss_GpuResources_syncDefaultStream"
  c_faiss_GpuResources_syncDefaultStream :: FaissGpuResources -> CInt -> IO CInt

foreign import ccall unsafe "faiss_GpuResources_syncDefaultStreamCurrentDevice"
  c_faiss_GpuResources_syncDefaultStreamCurrentDevice :: FaissGpuResources -> IO CInt

foreign import ccall unsafe "faiss_GpuResources_getAsyncCopyStreamCurrentDevice"
  c_faiss_GpuResources_getAsyncCopyStreamCurrentDevice :: 
    FaissGpuResources -> Ptr CudaStream -> IO CInt

-- Provider API
foreign import ccall unsafe "faiss_GpuResourcesProvider_getResources"
  c_faiss_GpuResourcesProvider_getResources :: 
    FaissGpuResourcesProvider -> Ptr FaissGpuResources -> IO CInt
