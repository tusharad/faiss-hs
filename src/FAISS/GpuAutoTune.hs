module FAISS.GpuAutoTune
  ( indexGpuToCpu
  , indexCpuToGpu
  , indexCpuToGpuWithOptions
  , indexCpuToGpuMultiple
  , indexCpuToGpuMultipleWithOptions
  ) where

import FAISS.Internal.GpuAutoTune
import FAISS.Internal.GpuClonerOptions
import FAISS.Internal.GpuResources
import FAISS.Internal.Index
import Foreign

-- | Convert GPU index to CPU index
indexGpuToCpu :: FaissIndex -> IO (Either String FaissIndex)
indexGpuToCpu gpuIndex = alloca $ \ptr -> do
  ret <- c_faiss_index_gpu_to_cpu gpuIndex ptr
  if ret == 0
    then Right <$> peek ptr
    else return $ Left "Failed to convert GPU index to CPU"

-- | Convert CPU index to GPU (single device, default options)
indexCpuToGpu ::
  FaissGpuResourcesProvider -> Int -> FaissIndex -> IO (Either String FaissIndex)
indexCpuToGpu provider device cpuIndex = alloca $ \ptr -> do
  ret <- c_faiss_index_cpu_to_gpu provider (fromIntegral device) cpuIndex ptr
  if ret == 0
    then Right <$> peek ptr
    else return $ Left "Failed to convert CPU index to GPU"

-- | Convert CPU index to GPU with options
indexCpuToGpuWithOptions ::
  FaissGpuResourcesProvider ->
  Int ->
  FaissIndex ->
  FaissGpuClonerOptions ->
  IO (Either String FaissIndex)
indexCpuToGpuWithOptions provider device cpuIndex options =
  alloca $ \ptr -> do
    ret <-
      c_faiss_index_cpu_to_gpu_with_options
        provider
        (fromIntegral device)
        cpuIndex
        options
        ptr
    if ret == 0
      then Right <$> peek ptr
      else return $ Left "Failed to convert CPU index to GPU (with options)"

-- | Convert CPU index to GPU on multiple devices (replicated)
indexCpuToGpuMultiple ::
  [FaissGpuResourcesProvider] ->
  [Int] ->
  FaissIndex ->
  IO (Either String FaissIndex)
indexCpuToGpuMultiple providers devices cpuIndex =
  withArray providers $ \provPtr ->
    withArray (map fromIntegral devices) $ \devPtr ->
      alloca $ \outPtr -> do
        let n = fromIntegral (length devices)
        ret <- c_faiss_index_cpu_to_gpu_multiple provPtr devPtr n cpuIndex outPtr
        if ret == 0
          then Right <$> peek outPtr
          else return $ Left "Failed to convert CPU index to multi-GPU index"

-- | Convert CPU index to GPU on multiple devices with cloner options
indexCpuToGpuMultipleWithOptions ::
  [FaissGpuResourcesProvider] ->
  [Int] ->
  FaissIndex ->
  FaissGpuMultipleClonerOptions ->
  IO (Either String FaissIndex)
indexCpuToGpuMultipleWithOptions providers devices cpuIndex options =
  withArray providers $ \provPtr ->
    withArray (map fromIntegral devices) $ \devPtr ->
      alloca $ \outPtr -> do
        let size = fromIntegral (length devices)
        ret <-
          c_faiss_index_cpu_to_gpu_multiple_with_options
            provPtr
            size
            devPtr
            size
            cpuIndex
            options
            outPtr
        if ret == 0
          then Right <$> peek outPtr
          else return $ Left "Failed to convert CPU index to multi-GPU index (with options)"
