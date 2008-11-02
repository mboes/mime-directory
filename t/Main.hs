module Main where

import qualified Data.ByteString.Lazy as B
import Codec.MIME.ContentType.Text.Directory
import System.IO
import System.Environment
import System.Exit

printUsage = putStrLn "vcard2vcard [FILE]" >> exitFailure

main = do
  args <- getArgs
  (filename, handle) <-
      case args of
        [file] -> openFile file ReadMode >>= return . ((,) file)
        [] -> return ("<stdin>", stdin)
        _ -> printUsage
  input <- B.hGetContents handle
  let dir = parseDirectory' pa_text input :: [Property Rfc2425Value]
  putStr $ show dir
