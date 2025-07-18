{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Langchain.VectorStore.Faiss
Description : Faiss-backed vector store for LangChain Haskell
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

This module provides a Faiss-backed implementation of the `VectorStore` typeclass.
It uses the `faiss-hs` library to create and manage a Faiss index for efficient
similarity searches on high-dimensional vectors generated from documents.

This implementation stores the original documents in a Haskell `Map` and uses the
Faiss index to store and search the corresponding vector embeddings.

Example usage:

@
-- 1. Initialize the embedding model and Faiss store
let ollamaEmbed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing Nothing
eFaissStore <- emptyFaissStore ollamaEmbed 768 MetricL2 -- 768 is the dimension for nomic-embed-text

case eFaissStore of
  Left err -> putStrLn $ "Error: " ++ err
  Right faissStore -> do
    -- 2. Add documents to the store
    let docs = [Document "Hello world" mempty, Document "Haskell is a functional language" mempty]
    eUpdatedStore <- addDocuments faissStore docs
    case eUpdatedStore of
      Left err -> putStrLn $ "Error: " ++ err
      Right updatedStore -> do
        -- 3. Perform a similarity search
        eResults <- similaritySearch updatedStore "What is a functional language?" 1
        case eResults of
          Left err -> putStrLn $ "Error: " ++ err
          Right results -> print results
          -- Expected output: Right [Document {pageContent = "Haskell is a functional language", ...}]
@
-}
module Langchain
  ( -- * Faiss Vector Store
    Faiss (..)

    -- * Constructors
  , emptyFaissStore
  , fromDocuments
  , runApp 
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified FAISS.Index as I
import qualified FAISS.IndexFlat as IF
import Langchain.Chain.RetrievalQA
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.Embeddings.Ollama (OllamaEmbeddings (..))
import Langchain.VectorStore.Core (VectorStore (..))
import Langchain.LLM.Ollama (Ollama(Ollama))
import Langchain.Retriever.Core 
import Langchain.Runnable.Core 

{- | A Faiss-backed vector store.
It holds an embedding model, the Faiss index itself, a map to store the
original documents, and the dimension of the vectors.
-}
data (Embeddings m) => Faiss m = Faiss
  { embeddingModel :: m
  -- ^ The model used to generate embeddings for documents and queries.
  , faissIndex :: I.FaissIndex
  -- ^ The underlying Faiss index that stores the vectors.
  , docstore :: Map.Map I.IdxT Document
  -- ^ A map from Faiss's integer IDs to the original `Document`.
  , indexDimension :: Int
  -- ^ The dimensionality of the vectors stored in the index.
  }

{- | Creates a new, empty Faiss vector store.
You must provide an embedding model, the dimension of the vectors it produces,
and the desired metric for similarity (e.g., `MetricL2` or `MetricInnerProduct`).
-}
emptyFaissStore ::
  (Embeddings m) =>
  m ->
  Int ->
  I.FaissMetricType ->
  IO (Either String (Faiss m))
emptyFaissStore model dim metric = do
  -- Create a simple IndexFlat, which is a good starting point.
  eIndex <- IF.indexFlatNewWith dim metric
  case eIndex of
    Left err -> return $ Left $ "Failed to create Faiss index: " ++ err
    Right index -> return $ Right $ Faiss model index Map.empty dim

{- | Creates a Faiss vector store from a list of documents.
This is a convenience function that initializes a store and adds the documents.
-}
fromDocuments ::
  (Embeddings m) =>
  m ->
  [Document] ->
  Int ->
  I.FaissMetricType ->
  IO (Either String (Faiss m))
fromDocuments model docs dim metric = do
  eStore <- emptyFaissStore model dim metric
  case eStore of
    Left err -> return $ Left err
    Right store -> addDocuments store docs

instance (Embeddings m) => VectorStore (Faiss m) where
  -- \| Adds documents to the Faiss vector store.
  -- It first generates embeddings for the documents and then adds them to the index.
  addDocuments faissStore@Faiss {..} docs = do
    if null docs
      then return $ Right faissStore
      else do
        -- Generate embeddings for the new documents
        eEmbeddings <- embedDocuments embeddingModel docs
        case eEmbeddings of
          Left err -> return $ Left $ "Failed to embed documents: " ++ err
          Right embeddings -> do
            -- Check if the embedding dimension matches the index dimension
            case embeddings of
              (firstVec : _)
                | length firstVec /= indexDimension ->
                    return $
                      Left $
                        "Embedding dimension mismatch. Index: "
                          ++ show indexDimension
                          ++ ", Got: "
                          ++ show (length firstVec)
              _ -> do
                -- Get the current size of the index to determine the new IDs
                ntotal <- I.indexGetNTotal faissIndex
                let startId = fromIntegral ntotal

                -- Add the vectors to the Faiss index
                eAddResult <- I.indexAdd faissIndex embeddings
                case eAddResult of
                  Left err ->
                    return $
                      Left $
                        "Failed to add vectors to Faiss index: " ++ err
                  Right () -> do
                    -- Create a map for the new documents with their new Faiss IDs
                    let newIds = [startId ..]
                        newEntries = Map.fromList $ zip newIds docs
                        -- Update the docstore
                        updatedDocstore = Map.union docstore newEntries
                    return $ Right $ faissStore {docstore = updatedDocstore}

  -- \| Deleting documents from a Faiss index is a complex operation that requires
  -- rebuilding or specific index types. This feature is not yet supported in this binding.
  delete _ _ =
    return $ Left "Deleting documents from the Faiss vector store is not yet implemented."

  -- \| Performs a similarity search using a text query.
  -- It embeds the query and then calls `similaritySearchByVector`.
  similaritySearch faissStore@Faiss {..} query k = do
    eQueryEmbedding <- embedQuery embeddingModel query
    case eQueryEmbedding of
      Left err -> return $ Left $ "Failed to embed query: " ++ err
      Right queryVec -> similaritySearchByVector faissStore queryVec k

  -- \| Performs a similarity search using a query vector.
  -- It searches the Faiss index for the `k` nearest neighbors and retrieves the
  -- corresponding documents from the docstore.
  similaritySearchByVector Faiss {..} queryVec k = do
    -- Ensure the query vector has the correct dimension
    if length queryVec /= indexDimension
      then
        return $
          Left $
            "Query vector dimension mismatch. Index: "
              ++ show indexDimension
              ++ ", Got: "
              ++ show (length queryVec)
      else do
        -- Search the index
        eSearchResult <- I.indexSearch faissIndex [queryVec] k
        case eSearchResult of
          Left err -> return $ Left $ "Faiss search failed: " ++ err
          Right (_, labels) -> do
            -- Look up the documents using the returned labels (IDs)
            let foundDocs = mapMaybe (\x -> fromIntegral x `Map.lookup` docstore) labels
            return $ Right foundDocs

runApp :: IO ()
runApp = do
  -- 1. Setup Models
  -- Define the embedding model to convert text to vectors.
  -- "nomic-embed-text" is a good default choice from Ollama.
  let embeddingModel =
        OllamaEmbeddings
          { model = "nomic-embed-text"
          , defaultTruncate = Nothing
          , defaultKeepAlive = Nothing
          }

  -- Define the Language Model for generating answers.
  -- "gemma3" is a powerful and fast model.
  let llm = Ollama "gemma3" []

  -- The dimension of vectors produced by "nomic-embed-text" is 768.
  -- This is a crucial parameter for initializing the Faiss index.
  let vectorDimension = 768

  -- 2. Initialize Faiss Vector Store
  -- We'll use the L2 distance metric for finding the nearest neighbors.
  putStrLn "Initializing Faiss vector store..."
  eFaissStore <- emptyFaissStore embeddingModel vectorDimension I.MetricL2
  case eFaissStore of
    Left err -> putStrLn $ "Error initializing Faiss store: " ++ err
    Right faissStore -> do
      -- 3. Add Documents
      -- These are the documents we want to be able to ask questions about.
      let docs =
            [ Document "Haskell is a purely functional programming language." Map.empty
            , Document "Tushar Kapoor is 24 years old." Map.empty
            , Document "Photosynthesis is the process by which plants make their own food." 
                        Map.empty
            ]

      putStrLn "Adding documents to the store..."
      eUpdatedStore <- addDocuments faissStore docs
      case eUpdatedStore of
        Left err -> putStrLn $ "Error adding documents: " ++ err
        Right updatedStore -> do
          -- 4. Create a Retriever
          -- The retriever is responsible for fetching relevant documents 
          -- from the vector store.
          let retriever = VectorStoreRetriever updatedStore

          -- 5. Create the RetrievalQA Chain
          -- This chain combines the retriever and the LLM into a single runnable component.
          let qaChain =
                RetrievalQA
                  { llm = llm
                  , llmParams = Nothing
                  , retriever = retriever
                  , prompt = defaultQAPrompt
                  }

          -- 6. Ask a Question
          let question = "How old is Tushar Kapoor?"
          putStrLn $ "\nAsking question: " ++ show question

          -- Invoke the chain. It will first retrieve relevant documents 
          -- (i.e., "The capital of France is Paris.")
          -- and then pass them as context to the LLM to generate a final answer.
          eResult <- invoke qaChain question
          case eResult of
            Left err -> putStrLn $ "Error during QA chain invocation: " ++ err
            Right answer -> do
              putStrLn "\nReceived answer:"
              print answer
