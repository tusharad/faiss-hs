{-# LANGUAGE ForeignFunctionInterface #-}

module FAISS.Internal.StandardGpuResources where

import Foreign
import Foreign.C.Types

-- | Opaque pointer type
data FaissStandardGpuResources_
type FaissStandardGpuResources = Ptr FaissStandardGpuResources_

-- Constructor
foreign import ccall unsafe "faiss_StandardGpuResources_new"
  c_faiss_StandardGpuResources_new :: Ptr FaissStandardGpuResources -> IO CInt

-- Destructor
foreign import ccall unsafe "&faiss_StandardGpuResources_free"
  c_faiss_StandardGpuResources_free :: FunPtr (FaissStandardGpuResources -> IO ())

-- Disable temp memory
foreign import ccall unsafe "faiss_StandardGpuResources_noTempMemory"
  c_faiss_StandardGpuResources_noTempMemory :: FaissStandardGpuResources -> IO CInt

-- Set temp memory
foreign import ccall unsafe "faiss_StandardGpuResources_setTempMemory"
  c_faiss_StandardGpuResources_setTempMemory :: 
    FaissStandardGpuResources -> CSize -> IO CInt

-- Set pinned memory size
foreign import ccall unsafe "faiss_StandardGpuResources_setPinnedMemory"
  c_faiss_StandardGpuResources_setPinnedMemory :: 
    FaissStandardGpuResources -> CSize -> IO CInt

-- Set default stream for a specific device
foreign import ccall unsafe "faiss_StandardGpuResources_setDefaultStream"
  c_faiss_StandardGpuResources_setDefaultStream :: 
    FaissStandardGpuResources -> CInt -> Ptr () -> IO CInt

-- Set null stream for all devices
foreign import ccall unsafe "faiss_StandardGpuResources_setDefaultNullStreamAllDevices"
  c_faiss_StandardGpuResources_setDefaultNullStreamAllDevices :: 
    FaissStandardGpuResources -> IO CInt
