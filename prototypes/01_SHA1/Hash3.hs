{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Lazy as L
import System.Environment   
import System.IO (stdin)

-- see : http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html
-- and : http://therning.org/magnus/archives/315
foreign import ccall "sha1_util.h all_hash_chunks"
     c_sha1_stream :: String -> IO (Ptr Word8) -> IO CInt

main = do
   args <- getArgs
   let pouet = mallocArray 20
   c_sha1_stream stdin pouet
   print pouet
   
{-
   hashes <- hashAFile 0 $ head args 
   print hashes

-- Offset + SHA1.
data HashPart = HashPart Int [Word8]   
instance Show HashPart where
   show (HashPart offset digest) = show offset ++ " - " ++ showDigest digest

-- Take a chunk size (in Byte) and a file path.
-- Return a list of hashes corresponding to each chunk of data.
hashAFile :: Int -> FilePath -> IO [HashPart]
hashAFile size path = do
   content <- L.readFile path
   return $ [HashPart 0 (sha1 content)]
   
-}