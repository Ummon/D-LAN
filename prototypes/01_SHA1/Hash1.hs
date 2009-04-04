{-
This file will test the speed of the package 'SHA' from hackage.
Usage : 
   ghc --make Hash1.hs
   time ./Hash1 01.wmv
-}

import Data.Digest.Pure.SHA (Digest(..), sha1, bytestringDigest, showDigest)
import qualified Data.ByteString.Lazy as L
import System.Environment   

main = do
   args <- getArgs
   hashes <- hashAFile 0 $ head args 
   print hashes

-- Offset + SHA1.
data HashPart = HashPart Int Digest   
instance Show HashPart where
   show (HashPart offset digest) = show offset ++ " - " ++ showDigest digest

-- Take a chunk size (in Byte) and a file path.
-- Return a list of hashes corresponding to each chunk of data.
hashAFile :: Int -> FilePath -> IO [HashPart]
hashAFile size path = do
   content <- L.readFile path
   return $ [HashPart 0 (sha1 content)]