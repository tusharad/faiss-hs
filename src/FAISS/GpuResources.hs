module FAISS.GpuResources
  ( FaissGpuResources
  , FaissGpuResourcesProvider
  , initializeForDevice
  , syncDefaultStream
  , syncDefaultStreamCurrent
  ) where

import FAISS.Internal.GpuResources

-- | Pre-allocate resources for a device
initializeForDevice :: FaissGpuResources -> Int -> IO (Either String ())
initializeForDevice res dev = do
  ret <- c_faiss_GpuResources_initializeForDevice res (fromIntegral dev)
  pure $ if ret == 0 then Right () else Left "Failed to initialize GPU resources"

-- | Synchronize default stream for device
syncDefaultStream :: FaissGpuResources -> Int -> IO (Either String ())
syncDefaultStream res dev = do
  ret <- c_faiss_GpuResources_syncDefaultStream res (fromIntegral dev)
  pure $ if ret == 0 then Right () else Left "Failed to sync stream"

-- | Sync default stream on current device
syncDefaultStreamCurrent :: FaissGpuResources -> IO (Either String ())
syncDefaultStreamCurrent res = do
  ret <- c_faiss_GpuResources_syncDefaultStreamCurrentDevice res
  pure $ if ret == 0 then Right () else Left "Failed to sync stream (current device)"
