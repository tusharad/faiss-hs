module FAISS.StandardGpuResources
  ( FaissStandardGpuResources
  , newStandardGpuResources
  , noTempMemory
  , setTempMemory
  , setPinnedMemory
  , setDefaultStream
  , setDefaultNullStreamAllDevices
  ) where

import Foreign
import FAISS.Internal.StandardGpuResources

-- | Create a new StandardGpuResources object
newStandardGpuResources :: IO (Either String FaissStandardGpuResources)
newStandardGpuResources =
  alloca $ \ptr -> do
    ret <- c_faiss_StandardGpuResources_new ptr
    if ret == 0 then Right <$> peek ptr
    else return $ Left "Failed to create StandardGpuResources"

-- | Disable temporary memory usage
noTempMemory :: FaissStandardGpuResources -> IO (Either String ())
noTempMemory res = do
  ret <- c_faiss_StandardGpuResources_noTempMemory res
  return $ if ret == 0 then Right () else Left "Failed to disable temp memory"

-- | Set temp memory (in bytes)
setTempMemory :: FaissStandardGpuResources -> Int -> IO (Either String ())
setTempMemory res size = do
  ret <- c_faiss_StandardGpuResources_setTempMemory res (fromIntegral size)
  return $ if ret == 0 then Right () else Left "Failed to set temp memory size"

-- | Set pinned memory (in bytes)
setPinnedMemory :: FaissStandardGpuResources -> Int -> IO (Either String ())
setPinnedMemory res size = do
  ret <- c_faiss_StandardGpuResources_setPinnedMemory res (fromIntegral size)
  return $ if ret == 0 then Right () else Left "Failed to set pinned memory"

-- | Set a CUDA stream (as a raw pointer) for a device
setDefaultStream :: FaissStandardGpuResources -> Int -> Ptr () -> IO (Either String ())
setDefaultStream res device stream = do
  ret <- c_faiss_StandardGpuResources_setDefaultStream res (fromIntegral device) stream
  return $ if ret == 0 then Right () else Left "Failed to set default stream"

-- | Set all devices to use null stream
setDefaultNullStreamAllDevices :: FaissStandardGpuResources -> IO (Either String ())
setDefaultNullStreamAllDevices res = do
  ret <- c_faiss_StandardGpuResources_setDefaultNullStreamAllDevices res
  return $ if ret == 0 then Right () else Left "Failed to set null stream for all devices"
