# faiss-hs

### Haskell Bindings for Faiss

`faiss-hs` provides minimal Haskell bindings over Facebook's [Faiss](https://github.com/facebookresearch/faiss) library, a library for efficient similarity search and clustering of dense vectors.

-----

## ⚠️ Project Status & Disclaimer

Please be aware that these Haskell bindings are **incomplete**. The primary goal of this repository is to provide the necessary functions to support a Faiss vector store for the `langchain-hs` library ([https://github.com/tusharad/langchain-hs](https://github.com/tusharad/langchain-hs)).

Consequently, only the bindings essential for this vector store implementation have been added. There are no current plans to complete the full Faiss API or publish this library as a formal package on Hackage. You can find an example of the Faiss vector store usage in the `examples` directory.

Anyone interested in completing the bindings, taking over the project, or publishing it as a package is welcome and encouraged to do so.

-----

## 🚀 Simple Usage

Here is an example demonstrating how to create an `IndexFlatL2` index, add vectors to it, and perform a k-nearest neighbors search.

```haskell
module Main (main) where

import FAISS.Index
import FAISS.IndexFlat
import System.Random
import Text.Printf

-- Helper function to generate a matrix of random floats
-- For a real application, consider using a more robust library like mwc-random.
generateRandomMatrix :: Int -> Int -> IO [[Float]]
generateRandomMatrix n d = do
  gen <- newStdGen
  let randoms' = randomRs (0.0, 1.0) gen
      vectors = take (n * d) randoms'
  return $ chunksOf d vectors
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf m xs = let (h, t) = splitAt m xs in h : chunksOf m t

main :: IO ()
main = do
  let d = 64      -- dimension of the vectors
      n_db = 2000 -- number of vectors in the database
      n_q = 5     -- number of query vectors
      k = 5       -- number of nearest neighbors to find

  -- Generate some random data for our database and queries
  databaseMatrix <- generateRandomMatrix n_db d
  queryMatrix <- generateRandomMatrix n_q d

  putStrLn $ "Creating an IndexFlatL2 index with dimension " ++ show d

-- Create a new IndexFlatL2 index
  eRes <- indexFlatL2NewWith d
  case eRes of
    Left err -> putStrLn $ "Error creating index: " <> err
    Right faissIndex -> do
      putStrLn "Index created successfully."
      
      -- Check if the index is trained (Flat indexes don't require training)
      isTrained <- indexGetIsTrained faissIndex 
      putStrLn $ "Is trained: " ++ show isTrained

      -- Add the database vectors to the index
      putStrLn $ "Adding " ++ show n_db ++ " vectors to the index..."
      eAdd <- indexAdd faissIndex databaseMatrix 
      case eAdd of
        Left err -> putStrLn $ "Error adding vectors: " <> err
        Right () -> do
          -- Get the total number of vectors in the index
          totalVectors <- indexGetNTotal faissIndex
          putStrLn $ "Total vectors in index: " ++ show totalVectors

          -- Search for the k nearest neighbors for each query vector 
          putStrLn $ "Searching for " ++ show k ++ " nearest neighbors for " ++ show n_q ++ " queries..."
          eSearch <- indexSearch faissIndex queryMatrix k 
          case eSearch of
            Left err -> putStrLn $ "Error during search: " <> err
            Right (distances, labels) -> do
              putStrLn "Search completed successfully. Top 5 query results:"
              let distanceChunks = chunksOf k distances
                  labelChunks = chunksOf k labels
              -- Nicely print the first few query results
              mapM_ (printQueryResult) (zip [0..] (zip distanceChunks labelChunks))

      -- Free the index from memory
      indexFree faissIndex
      putStrLn "Index freed. All good."

-- A small helper to pretty-print search results
printQueryResult :: (Int, ([[Float]], [[Int]])) -> IO ()
printQueryResult (i, (dists, lbls)) = do
    printf "Query %d:\n" i
    mapM_ (\(dist, lbl) -> printf "  Label: %-5d | Distance: %.4f\n" lbl dist) (zip (head lbls) (head dists))

```

-----

## 📦 Installation

### Prerequisites

It is **required** to have the `faiss-c` shared library installed on your machine.

1.  Clone or download the Faiss repository.
2.  Follow the official `install.md` guide: [https://github.com/facebookresearch/faiss/blob/main/INSTALL.md](https://github.com/facebookresearch/faiss/blob/main/INSTALL.md)
3.  When configuring the build, make sure to enable the C API and shared libraries:
    ```bash
    cmake -B build . -DFAISS_ENABLE_C_API=ON -DBUILD_SHARED_LIBS=ON
    ```
4.  This will produce the dynamic library `faiss_c` (`c_api/libfaiss_c.so` on Linux, `c_api/libfaiss_c.dylib` on macOS).
5.  Install this library in a standard system path where it can be found by the dynamic linker (e.g., `/usr/local/lib`).

### Recommended Setup (Nix)

If you have Nix installed, you can simply run `nix-shell` from the project root. The provided `default.nix` file will automatically download Faiss, build it with the correct flags, and make the `libfaiss_c` library available in your shell environment.

-----

## Versioning, Contributions, and License

  * **Faiss Version**: These bindings are currently built against **Faiss v1.11.0**.
  * **Contributions**: Feel free to fork the repository and submit a pull request or open an issue.
  * **License**: This project is licensed under the **MIT License**.
