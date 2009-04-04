import Data.Digest.SHA1
import qualified Data.ByteString.Lazy as L
import System.Environment   

main = do
   args <- getArgs
   hashes <- hashAFile 0 $ head args 
   print hashes

-- Offset + SHA1.
data HashPart = HashPart Int Word160   
instance Show HashPart where
   show (HashPart offset digest) = show offset ++ " - " ++ (show digest)

-- Take a chunk size (in Byte) and a file path.
-- Return a list of hashes corresponding to each chunk of data.
hashAFile :: Int -> FilePath -> IO [HashPart]
hashAFile size path = do
   content <- L.readFile path
   return $ [HashPart 0 (hash $ L.unpack content)]