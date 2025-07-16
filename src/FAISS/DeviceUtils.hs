module FAISS.DeviceUtils
  ( getNumGPUs
  , gpuProfilerStart
  , gpuProfilerStop
  , gpuSyncAllDevices
  ) where

import Foreign
import FAISS.Internal.DeviceUtils

-- | Get the number of available GPU devices
getNumGPUs :: IO (Either String Int)
getNumGPUs = alloca $ \ptr -> do
  ret <- c_faiss_get_num_gpus ptr
  if ret == 0
    then Right . fromIntegral <$> peek ptr
    else return $ Left "Failed to get number of GPUs"

-- | Start the CUDA profiler
gpuProfilerStart :: IO (Either String ())
gpuProfilerStart = do
  ret <- c_faiss_gpu_profiler_start
  return $ if ret == 0 then Right () else Left "Failed to start CUDA profiler"

-- | Stop the CUDA profiler
gpuProfilerStop :: IO (Either String ())
gpuProfilerStop = do
  ret <- c_faiss_gpu_profiler_stop
  return $ if ret == 0 then Right () else Left "Failed to stop CUDA profiler"

-- | Synchronize CPU with all devices (cudaDeviceSynchronize across all)
gpuSyncAllDevices :: IO (Either String ())
gpuSyncAllDevices = do
  ret <- c_faiss_gpu_sync_all_devices
  return $ if ret == 0 then Right () else Left "Failed to sync all devices"
