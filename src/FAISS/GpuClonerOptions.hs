module FAISS.GpuClonerOptions
  ( FaissGpuClonerOptions
  , FaissGpuMultipleClonerOptions
  , FaissGpuIndexConfig
  , FaissIndicesOptions(..)
  , newGpuClonerOptions
  , newGpuMultipleClonerOptions
  , getDevice
  ) where

import Foreign
import FAISS.Internal.GpuClonerOptions

-- | Construct a new GpuClonerOptions
newGpuClonerOptions :: IO (Either String FaissGpuClonerOptions)
newGpuClonerOptions =
  alloca $ \ptr -> do
    ret <- c_faiss_GpuClonerOptions_new ptr
    if ret == 0 then
        Right <$> peek ptr 
    else return $ Left "Failed to create GpuClonerOptions"

-- | Construct a new GpuMultipleClonerOptions
newGpuMultipleClonerOptions :: IO (Either String FaissGpuMultipleClonerOptions)
newGpuMultipleClonerOptions =
  alloca $ \ptr -> do
    ret <- c_faiss_GpuMultipleClonerOptions_new ptr
    if ret == 0 then 
        Right <$> peek ptr else return $ Left "Failed to create GpuMultipleClonerOptions"

-- | Get the target GPU device from the config
getDevice :: FaissGpuIndexConfig -> IO Int
getDevice cfg = fromIntegral <$> c_get_device cfg
