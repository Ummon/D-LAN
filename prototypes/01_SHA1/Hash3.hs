{-# LANGUAGE ForeignFunctionInterface #-}

{-
This file will test the speed of a binding with sha1.h and sha1_util.h.
Usage : 
   TODO...
-}

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
   