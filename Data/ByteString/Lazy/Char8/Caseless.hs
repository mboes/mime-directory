-- | A variant of ByteString where strings differing in the case of some of
-- its characters are identified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ByteString.Lazy.Char8.Caseless
    ( ByteString
    , sensitize
    , unsensitize
    , concat
    , intercalate
    , pack
    , unpack
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toLower)
import GHC.Exts (IsString)
import Data.Monoid
#if defined(__GLASGOW_HASKELL__)
import Data.Typeable (Typeable)
import Data.Data (Data)
#endif
import Prelude hiding (concat)

-- | Wrapper for case insensitive strings.
newtype ByteString = I B.ByteString
    deriving ( Eq, Ord, Show, Semigroup, Monoid
#if defined(__GLASGOW_HASKELL__)
             , IsString, Data, Typeable
#endif
             )

-- | Inject a bytestring into a case insensitive bytestring. Note that the case
-- of all letters in the ByteString is folded to lower case.
unsensitize :: B.ByteString -> ByteString
unsensitize = I . B.map toLower

-- | Project back to a regular bytestring.
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
