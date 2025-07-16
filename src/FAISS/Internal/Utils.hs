module FAISS.Internal.Utils
  ( withFloatArray
  , withIdxArray
  , peekFloatArray
  , peekIdxArray
  , withOptionalArray
  ) where

import FAISS.Internal.Index
import Foreign
import Foreign.C.Types

-- | Marshal a list of Floats to a C array
withFloatArray :: [Float] -> (Ptr CFloat -> IO a) -> IO a
withFloatArray xs f = withArray (map realToFrac xs) f

-- | Marshal a list of Ints to a C array of IdxT
withIdxArray :: [Int] -> (Ptr IdxT -> IO a) -> IO a
withIdxArray xs f = withArray (map fromIntegral xs) f

-- | Peek a C array of CFloats and convert to [Float]
peekFloatArray :: Int -> Ptr CFloat -> IO [Float]
peekFloatArray n ptr = map realToFrac <$> peekArray n ptr

-- | Peek a C array of IdxT and convert to [Int]
peekIdxArray :: Int -> Ptr IdxT -> IO [Int]
peekIdxArray n ptr = map fromIntegral <$> peekArray n ptr

withOptionalArray :: Storable a => Maybe [a] -> (Ptr a -> IO b) -> IO b
withOptionalArray Nothing f = f nullPtr
withOptionalArray (Just xs) f = withArray xs f
