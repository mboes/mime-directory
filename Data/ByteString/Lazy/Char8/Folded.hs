-- | A variant of ByteString where strings differing in the case of some of
-- its characters are identified.
module Data.ByteString.Lazy.Char8.Folded
    (ByteString, sensitize, unsensitize, concat, intercalate, pack, unpack) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toLower)
import GHC.Exts (IsString)
import Data.Monoid (Monoid)
import Data.Generics.Basics (Data, Typeable)
import Prelude hiding (concat)

-- | Wrapper for case insensitive strings.
newtype ByteString = I B.ByteString
    deriving (Data, Eq, IsString, Ord, Show, Monoid, Typeable)

unsensitize :: B.ByteString -> ByteString
unsensitize = I . B.map toLower

sensitize :: ByteString -> B.ByteString
sensitize (I str) = str

concat :: [ByteString] -> ByteString
concat = I . B.concat . map sensitize

intercalate :: ByteString -> [ByteString] -> ByteString
intercalate sep = I . B.intercalate (sensitize sep) . map sensitize

pack :: String -> ByteString
pack = unsensitize . B.pack

unpack :: ByteString -> String
unpack = B.unpack . sensitize
