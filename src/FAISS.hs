{- |
Module      : FAISS
Description : Re-export of all FAISS modules
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Haskell bindings over facebook's FAISS library. This module re-exports all
functionality from the various submodules for convenience.
-}
module FAISS
  ( -- * Core Index Types and Operations (from FAISS.Index)
    Index.FaissIDSelector
  , Index.FaissSearchParameters
  , Index.FaissIndex
  , Index.FaissMetricType (..)
  , Index.FaissRangeSearchResult
  , Index.IdxT
  , Index.indexAdd
  , Index.indexAddWithIds
  , Index.indexSearch
  , Index.indexSearchWithParams
  , Index.searchParametersNew
  , Index.indexTrain
  , Index.indexAssign
  , Index.indexReset
  , Index.indexRemoveIds
  , Index.indexReconstruct
  , Index.indexReconstructN
  , Index.indexComputeResidual
  , Index.indexComputeResidualN
  , Index.indexSaCodeSize
  , Index.indexSaEncode
  , Index.indexSaDecode
  , Index.indexFree
  , Index.indexFileWrite
  , Index.idSelectorFree
  , Index.idSelectorRangeFree
  , Index.searchParametersFree
  , Index.indexGetD
  , Index.indexGetIsTrained
  , Index.indexGetNTotal
  , Index.indexGetMetricType
  , Index.indexGetVerbose
  , Index.indexSetVerbose

    -- * Index Factory (from FAISS.IndexFactory)
  , IndexFactory.indexFactory

    -- * IndexFlat Implementations (from FAISS.IndexFlat)
  , IndexFlat.indexFlatNew
  , IndexFlat.indexFlatNewWith
  , IndexFlat.indexFlatL2New
  , IndexFlat.indexFlatL2NewWith
  , IndexFlat.indexFlatIPNewWith
  , IndexFlat.indexFlatIPNew
  , IndexFlat.indexRefineFlatNew
  , IndexFlat.indexFlat1DNew
  , IndexFlat.indexFlat1DNewWith
  , IndexFlat.indexFlat1DUpdatePermutation
  , IndexFlat.indexFlatComputeDistanceSubset

    -- * IndexIVFFlat Implementations (from FAISS.IndexIVFFlat)
  , IndexIVFFlat.indexIVFFlatNew
  , IndexIVFFlat.indexIVFFlatNewWith
  , IndexIVFFlat.indexIVFFlatNewWithMetric
  , IndexIVFFlat.indexIVFFlatAddCore
  , IndexIVFFlat.indexIVFFlatUpdateVectors
  , IndexIVFFlat.indexIVFFlatSetNProbe

    -- * GPU Utilities (from FAISS.GpuAutoTune)
  , GpuAutoTune.indexGpuToCpu
  , GpuAutoTune.indexCpuToGpu
  , GpuAutoTune.indexCpuToGpuWithOptions
  , GpuAutoTune.indexCpuToGpuMultiple
  , GpuAutoTune.indexCpuToGpuMultipleWithOptions

    -- * GPU Cloner Options (from FAISS.GpuClonerOptions)
  , GpuClonerOptions.FaissGpuClonerOptions
  , GpuClonerOptions.FaissGpuMultipleClonerOptions
  , GpuClonerOptions.FaissGpuIndexConfig
  , GpuClonerOptions.FaissIndicesOptions (..)
  , GpuClonerOptions.newGpuClonerOptions
  , GpuClonerOptions.newGpuMultipleClonerOptions
  , GpuClonerOptions.getDevice

    -- * GPU Resources (from FAISS.GpuResources)
  , GpuResources.FaissGpuResources
  , GpuResources.FaissGpuResourcesProvider
  , GpuResources.initializeForDevice
  , GpuResources.syncDefaultStream
  , GpuResources.syncDefaultStreamCurrent

    -- * Standard GPU Resources (from FAISS.StandardGpuResources)
  , StandardGpuResources.FaissStandardGpuResources
  , StandardGpuResources.newStandardGpuResources
  , StandardGpuResources.noTempMemory
  , StandardGpuResources.setTempMemory
  , StandardGpuResources.setPinnedMemory
  , StandardGpuResources.setDefaultStream
  , StandardGpuResources.setDefaultNullStreamAllDevices

    -- * GPU Device Utilities (from FAISS.DeviceUtils)
  , DeviceUtils.getNumGPUs
  , DeviceUtils.gpuProfilerStart
  , DeviceUtils.gpuProfilerStop
  , DeviceUtils.gpuSyncAllDevices

    -- * Auxiliary Index Structures (from FAISS.AuxIndexStructures)
  , Aux.rangeSearchResultNew
  , Aux.rangeSearchResultNewWith
  , Aux.rangeSearchResultDoAllocation
  , Aux.getRangeSearchResultNQ
  , Aux.getRangeSearchResultBufferSize
  , Aux.getRangeSearchResultLims
  , Aux.getRangeSearchResultLabels
  , Aux.idSelectorIsMember
  , Aux.idSelectorRangeNew
  , Aux.idSelectorNotNew
  , Aux.idSelectorAndNew
  , Aux.idSelectorOrNew
  , Aux.idSelectorXOrNew
  ) where

import qualified FAISS.AuxIndexStructures as Aux
import qualified FAISS.DeviceUtils as DeviceUtils
import qualified FAISS.GpuAutoTune as GpuAutoTune
import qualified FAISS.GpuClonerOptions as GpuClonerOptions
import qualified FAISS.GpuResources as GpuResources
import qualified FAISS.Index as Index
import qualified FAISS.IndexFactory as IndexFactory
import qualified FAISS.IndexFlat as IndexFlat
import qualified FAISS.IndexIVFFlat as IndexIVFFlat
import qualified FAISS.StandardGpuResources as StandardGpuResources
