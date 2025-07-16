{-# LANGUAGE ForeignFunctionInterface #-}

module FAISS.Internal.DeviceUtils where

import Foreign
import Foreign.C.Types

-- | Get number of available GPUs
foreign import ccall unsafe "faiss_get_num_gpus"
  c_faiss_get_num_gpus :: Ptr CInt -> IO CInt

-- | Start CUDA profiler
foreign import ccall unsafe "faiss_gpu_profiler_start"
  c_faiss_gpu_profiler_start :: IO CInt

-- | Stop CUDA profiler
foreign import ccall unsafe "faiss_gpu_profiler_stop"
  c_faiss_gpu_profiler_stop :: IO CInt

-- | Synchronize all devices
foreign import ccall unsafe "faiss_gpu_sync_all_devices"
  c_faiss_gpu_sync_all_devices :: IO CInt
