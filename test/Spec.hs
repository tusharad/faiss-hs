import Control.Monad
import Data.Either
import Data.Maybe (fromMaybe, listToMaybe)
import FAISS
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain faissTests

-- Test data generators
testVectors2D :: [[Float]]
testVectors2D =
  [ [1.0, 2.0]
  , [3.0, 4.0]
  , [5.0, 6.0]
  , [7.0, 8.0]
  ]

{-
testVectors3D :: [[Float]]
testVectors3D =
  [ [1.0, 2.0, 3.0]
  , [4.0, 5.0, 6.0]
  , [7.0, 8.0, 9.0]
  , [10.0, 11.0, 12.0]
  ]
  -}

testVectorsLarge :: [[Float]]
testVectorsLarge =
  [ [i, (i + 1), (i + 2)]
  | i <- [1.0 .. 100.0]
  ]

queryVectors2D :: [[Float]]
queryVectors2D =
  [ [1.1, 2.1]
  , [3.1, 4.1]
  ]

queryVectors3D :: [[Float]]
queryVectors3D =
  [ [1.1, 2.1, 3.1]
  , [4.1, 5.1, 6.1]
  ]

{-
testIds :: [Int]
testIds = [100, 101, 102, 103]
-}

-- Test configuration
data TestConfig = TestConfig
  { tcDimension :: Int
  , tcVectors :: [[Float]]
  , tcQueryVectors :: [[Float]]
  , tcIndexDescription :: String
  , tcMetricType :: FaissMetricType
  , tcK :: Int
  }

config2D :: TestConfig
config2D =
  TestConfig
    { tcDimension = 2
    , tcVectors = testVectors2D
    , tcQueryVectors = queryVectors2D
    , tcIndexDescription = "Flat"
    , tcMetricType = MetricInnerProduct
    , tcK = 2
    }

{-
config3D :: TestConfig
config3D =
  TestConfig
    { tcDimension = 3
    , tcVectors = testVectors3D
    , tcQueryVectors = queryVectors3D
    , tcIndexDescription = "Flat"
    , tcMetricType = MetricInnerProduct
    , tcK = 2
    }
    -}

configLarge :: TestConfig
configLarge =
  TestConfig
    { tcDimension = 3
    , tcVectors = testVectorsLarge
    , tcQueryVectors = queryVectors3D
    , tcIndexDescription = "IVF100,Flat"
    , tcMetricType = MetricL2
    , tcK = 5
    }

-- Test suite structure
faissTests :: TestTree
faissTests =
  testGroup
    "Faiss Haskell Bindings"
    [ indexFactoryTests
    , indexPropertiesTests
    , indexOperationsTests
    , searchTests
    , advancedOperationsTests
    , errorHandlingTests
    ]

-- Index Factory Tests
indexFactoryTests :: TestTree
indexFactoryTests =
  testGroup
    "Index Factory Tests"
    [ testCase "Create Flat index 2D" $ do
        result <- indexFactory 2 "Flat" MetricL2
        result `shouldSatisfy` isRight
    , testCase "Create Flat index 3D" $ do
        result <- indexFactory 3 "Flat" MetricL2
        result `shouldSatisfy` isRight
    , testCase "Create IVF index" $ do
        result <- indexFactory 3 "IVF100,Flat" MetricL2
        result `shouldSatisfy` isRight
    , testCase "Invalid dimension should fail" $ do
        result <- indexFactory (-1) "Flat" MetricL2
        result `shouldSatisfy` isLeft
    ]

-- Index Properties Tests
indexPropertiesTests :: TestTree
indexPropertiesTests =
  testGroup
    "Index Properties Tests"
    [ testCase "Get dimension" $ testWithIndex config2D $ \idx -> do
        d <- indexGetD idx
        d @?= 2
    , testCase "Check initial training status" $ testWithIndex config2D $ \idx -> do
        isTrained <- indexGetIsTrained idx
        isTrained @?= True -- Flat index should be trained by default
    , testCase "Check initial vector count" $ testWithIndex config2D $ \idx -> do
        ntotal <- indexGetNTotal idx
        ntotal @?= 0
    , testCase "Get metric type" $ testWithIndex config2D $ \idx -> do
        metric <- indexGetMetricType idx
        metric @?= tcMetricType config2D
    , testCase "Get and set verbose level" $ testWithIndex config2D $ \idx -> do
        -- Get initial verbose level
        initialVerbose <- indexGetVerbose idx
        -- Set new verbose level
        indexSetVerbose idx 1
        newVerbose <- indexGetVerbose idx
        newVerbose @?= 1
        -- Reset to original
        indexSetVerbose idx initialVerbose
    ]

-- Index Operations Tests
indexOperationsTests :: TestTree
indexOperationsTests =
  testGroup
    "Index Operations Tests"
    [ testCase "Add vectors" $ testWithIndex config2D $ \idx -> do
        result <- indexAdd idx (tcVectors config2D)
        result `shouldSatisfy` isRight

        ntotal <- indexGetNTotal idx
        ntotal @?= length (tcVectors config2D)
    , {- TODO: Not supported
      , testCase "Add vectors with IDs" $ testWithIndex config2D $ \idx -> do
          result <- indexAddWithIds idx (tcVectors config2D) testIds
          result `shouldSatisfy` isRight

          ntotal <- indexGetNTotal idx
          ntotal @?= length (tcVectors config2D)
          -}
      testCase "Train index (IVF)" $ testWithIndex configLarge $ \idx -> do
        -- First check if training is needed
        isTrained <- indexGetIsTrained idx
        unless isTrained $ do
          result <- indexTrain idx (tcVectors configLarge)
          result `shouldSatisfy` isRight

          isTrainedAfter <- indexGetIsTrained idx
          isTrainedAfter @?= True
    , testCase "Reset index" $ testWithIndex config2D $ \idx -> do
        -- Add vectors first
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Check vectors were added
        ntotalBefore <- indexGetNTotal idx
        ntotalBefore @?= length (tcVectors config2D)

        -- Reset index
        resetResult <- indexReset idx
        resetResult `shouldSatisfy` isRight

        -- Check vectors were removed
        ntotalAfter <- indexGetNTotal idx
        ntotalAfter @?= 0
    ]

-- Search Tests
searchTests :: TestTree
searchTests =
  testGroup
    "Search Tests"
    [ testCase "Basic search" $ testWithIndex config2D $ \idx -> do
        -- Add vectors
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Search
        searchResult <- indexSearch idx (tcQueryVectors config2D) (tcK config2D)
        searchResult `shouldSatisfy` isRight
        case searchResult of
          Left err -> assertFailure err
          Right (distances, labels) -> do
            length distances @?= length (tcQueryVectors config2D) * tcK config2D
            length labels @?= length (tcQueryVectors config2D) * tcK config2D
    , {-
      , testCase "Search with parameters" $ testWithIndex config2D $ \idx -> do
          -- Add vectors
          addResult <- indexAdd idx (tcVectors config2D)
          addResult `shouldSatisfy` isRight

          -- Create search parameters
          searchParams <- searchParametersNew nullPtr -- Adjust based on your ID selector
          case searchParams of
            Left err -> assertFailure $ "Failed to create search parameters: " ++ err
            Right params -> do
              searchResult <-
                  indexSearchWithParams idx (tcQueryVectors config2D) (tcK config2D) params
              searchResult `shouldSatisfy` isRight
              -}
      testCase "Assignment" $ testWithIndex config2D $ \idx -> do
        -- Add vectors
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Assign
        assignResult <- indexAssign idx (tcQueryVectors config2D) (tcK config2D)
        assignResult `shouldSatisfy` isRight
        case assignResult of
          Left err -> assertFailure err
          Right labels ->
            length labels @?= length (tcQueryVectors config2D) * tcK config2D
    , testCase "Search returns valid distances" $ testWithIndex config2D $ \idx -> do
        -- Add vectors
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Search
        searchResult <- indexSearch idx (tcQueryVectors config2D) (tcK config2D)
        searchResult `shouldSatisfy` isRight
        case searchResult of
          Left err -> assertFailure err
          Right (distances, labels) -> do
            -- All distances should be non-negative for L2 metric
            all (>= 0) distances @?= True
            -- All labels should be valid indices
            all (\l -> l >= 0 && l < length (tcVectors config2D)) labels @?= True
    ]

-- Advanced Operations Tests
advancedOperationsTests :: TestTree
advancedOperationsTests =
  testGroup
    "Advanced Operations Tests"
    [ testCase "Reconstruct vector" $ testWithIndex config2D $ \idx -> do
        -- Add vectors
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Reconstruct first vector
        reconstructResult <- indexReconstruct idx 0 (tcDimension config2D)
        reconstructResult `shouldSatisfy` isRight

        case reconstructResult of
          Left err -> assertFailure err
          Right reconstructed -> do
            length reconstructed @?= tcDimension config2D
    , testCase "Reconstruct multiple vectors" $ testWithIndex config2D $ \idx -> do
        -- Add vectors
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Reconstruct first 2 vectors
        reconstructResult <- indexReconstructN idx 0 2 (tcDimension config2D)
        reconstructResult `shouldSatisfy` isRight

        case reconstructResult of
          Left err -> assertFailure err
          Right reconstructed -> do
            length reconstructed @?= 2 * tcDimension config2D
    , testCase "Compute residual" $ testWithIndex config2D $ \idx -> do
        -- Add vectors
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Compute residual for first vector
        let firstVector = fromMaybe [] $ listToMaybe (tcVectors config2D)
        residualResult <- indexComputeResidual idx firstVector 0
        residualResult `shouldSatisfy` isRight

        case residualResult of
          Left err -> assertFailure err
          Right residual -> do
            length residual @?= length firstVector
    , testCase "Compute residual for multiple vectors" $ testWithIndex config2D $ \idx -> do
        -- Add vectors
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Compute residuals
        let vectors = take 2 (tcVectors config2D)
        let keys = [0, 1]
        residualResult <- indexComputeResidualN idx vectors keys
        residualResult `shouldSatisfy` isRight

        case residualResult of
          Left err -> assertFailure err
          Right residuals -> length residuals @?= length vectors
    , testCase "Encode and decode" $ testWithIndex config2D $ \idx -> do
        -- Add vectors
        addResult <- indexAdd idx (tcVectors config2D)
        addResult `shouldSatisfy` isRight

        -- Get code size
        codeSizeResult <- indexSaCodeSize idx
        codeSizeResult `shouldSatisfy` isRight

        -- Encode vectors
        encodeResult <- indexSaEncode idx (tcVectors config2D)
        encodeResult `shouldSatisfy` isRight

        case (encodeResult, codeSizeResult) of
          (Right encoded, Right codeSize) -> do
            length encoded @?= length (tcVectors config2D) * codeSize
            decodeResult <-
              indexSaDecode
                idx
                encoded
                (length (tcVectors config2D))
                (tcDimension config2D)
            decodeResult `shouldSatisfy` isRight
            case decodeResult of
              Left err -> assertFailure err
              Right decoded -> do
                length decoded @?= length (tcVectors config2D) * tcDimension config2D
          _ -> assertFailure $ fromLeft "" encodeResult <> fromLeft "" codeSizeResult
    ]

-- Error Handling Tests
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ 
    {- TODO:
    testCase "Add vectors with wrong dimension" $ testWithIndex config2D $ \idx -> do
      let wrongDimVectors = [[1.0, 2.0, 3.0]] -- 3D vector to 2D index
      result <- indexAdd idx wrongDimVectors
      result `shouldSatisfy` isLeft
      -}
      {-
  , testCase "Search with wrong query dimension" $ testWithIndex config2D $ \idx -> do
      -- Add vectors first
      addResult <- indexAdd idx (tcVectors config2D)
      addResult `shouldSatisfy` isRight
      
      -- Search with wrong dimension
      let wrongQueryVectors = [[1.0, 2.0, 3.0]]
      searchResult <- indexSearch idx wrongQueryVectors 1
      searchResult `shouldSatisfy` isLeft
      -}
  testCase "Search with k=0" $ testWithIndex config2D $ \idx -> do
      -- Add vectors first
      addResult <- indexAdd idx (tcVectors config2D)
      addResult `shouldSatisfy` isRight
      
      -- Search with k=0
      searchResult <- indexSearch idx (tcQueryVectors config2D) 0
      searchResult `shouldSatisfy` isLeft
   {-   
  , testCase "Reconstruct invalid index" $ testWithIndex config2D $ \idx -> do
      -- Add vectors first
      addResult <- indexAdd idx (tcVectors config2D)
      addResult `shouldSatisfy` isRight
      
      -- Try to reconstruct invalid index
      reconstructResult <- indexReconstruct idx 999 (tcDimension config2D)
      reconstructResult `shouldSatisfy` isLeft
      -}
  ]

-- Helper Functions
testWithIndex :: TestConfig -> (FaissIndex -> IO ()) -> IO ()
testWithIndex config action = do
  indexResult <-
    indexFactory
      (tcDimension config)
      (tcIndexDescription config)
      (tcMetricType config)
  case indexResult of
    Left err -> assertFailure $ "Failed to create index: " ++ err
    Right idx -> action idx

shouldSatisfy :: (Show a) => a -> (a -> Bool) -> Assertion
shouldSatisfy actual predicate =
  unless (predicate actual) $
    assertFailure $
      "Expected condition not satisfied for: " ++ show actual
