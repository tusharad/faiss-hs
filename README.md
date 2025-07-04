# faiss-hs

Haskell bindings over Facebook's [faiss](https://github.com/facebookresearch/faiss) library.

## Simple Usage

```haskell
module Main (main) where

import FAISS.Index
import FAISS.Internal.Index
import FAISS.Internal.IndexFactory

main :: IO ()
main = do
  let d = 32
      n = 1000
      q = 100
      databaseMatrix = replicate n (replicate d 2.3)
      queryMatrix = replicate q (replicate d 3.3)
  eRes <- indexFactory d "Flat" MetricL2
  case eRes of
    Left err -> putStrLn $ "error: " <> err
    Right faissIndex -> do
      indexGetD faissIndex >>= print
      indexGetIsTrained faissIndex >>= print
      indexGetNTotal faissIndex >>= print
      indexGetMetricType faissIndex >>= print
      indexAdd faissIndex databaseMatrix
      indexGetNTotal faissIndex >>= print
      indexSearch faissIndex queryMatrix 3 >>= (\x -> print $ fst <$> x)
      putStrLn "all good"
```

You can find the collection of examples in `examples` directory.

## Requirement of faiss-c library.

It is required to have installed the faiss-c library on your machine before you use faiss-hs.
You can install it mannually by following [install.md](https://github.com/facebookresearch/faiss/blob/main/INSTALL.md); Make sure to enable
the variables `FAISS_ENABLE_C_API` and `BUILD_SHARED_LIBS`. This will result in the dynamic library faiss_c ("c_api/libfaiss_c.so" on Linux),
which needs to be installed in a place where your system will pick up (in Linux, try somewhere in the LD_LIBRARY_PATH environment variable,
such as "/usr/lib", or try adding a new path to this variable. For mac users, this means placing it in /usr/local/lib/libfaiss_c.dylib).

### Nix installation (recommended)

If you have nix installed; you can try running `nix-shell`. The `default.nix` will install faiss along with libfaiss_c and install it in `/user/local/lib` automatically.

## faiss Version

Currently faiss-hs is binded over faiss-1.11.0; it will be updated once newer version comes out.

## Contributions

Feel free to fork and raise a PR or raise issue.

## Licence

The project is licence under MIT.
