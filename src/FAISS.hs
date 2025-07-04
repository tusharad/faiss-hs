{- |
Module      : FAISS
Description : Re-export of all FAISS modules
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Haskell bindings over facebook's FAISS library.
-}
module FAISS
  ( -- * Basic operations over index
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

    -- ** The standalone codec interface
  , Index.indexSaCodeSize
  , Index.indexSaEncode
  , Index.indexSaDecode

    -- ** Get Index properties
  , Index.indexGetD
  , Index.indexGetIsTrained
  , Index.indexGetNTotal
  , Index.indexGetMetricType
  , Index.indexGetVerbose

    -- ** Set Index properties
  , Index.indexSetVerbose

    -- * IndexFactory
  , IndexFactory.indexFactory
  ) where

import qualified FAISS.Index as Index
import qualified FAISS.IndexFactory as IndexFactory
