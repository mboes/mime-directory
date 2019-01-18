-- |
-- Copyright   : (c) 2008 Mathieu Boespflug
-- License     : LGPL
-- Maintainer  : mboes@tweag.net
-- Stability   : experimental
-- Portability : non-portable
--
-- Library for parsing and generating the text/directory mime content type.
-- This library implements all the required mechanisms in RFC 2425, which other
-- libraries may use to implement parsing and generating specific profiles,
-- such as vCard.

{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Codec.MIME.ContentType.Text.Directory
    ( -- * Types
      Directory
    , Property(..)
    , Type(..)
    , Parameter(..)
    , Value(..)
    , Rfc2425Value
    , PrintValue(..)
    , ValueParser
    , nakedType
    , (@@)
    , lookupParameter
    -- * Encoding\/decoding values
    , decodeValue
    , encodeValue
    , escape
    -- * Parsing
    , parseDirectory
    , parseDirectory'
    , fromList
    , groupByBeginEnd
    -- ** Value Parsers
    , pa_URI
    , pa_text
    , pa_date
    , pa_time
    , pa_dateTime
    , pa_integer
    , pa_boolean
    , pa_float
    , pa_textList
    -- ** Value parser combinators
    , many
    -- * Printing
    , printDirectory
    , printDirectory'
    , printProperty
    ) where

import Control.Applicative hiding (many)
import Data.Time (Day, DiffTime, ParseTime, UTCTime, utctDayTime)
#if MIN_VERSION_time(1,5,0)
import Data.Time (TimeLocale, defaultTimeLocale, iso8601DateFormat, parseTimeOrError)
#else
import Data.Time (readTime)
#endif
import Data.Maybe (fromJust)
import Text.Regex.PCRE.ByteString.Lazy
import qualified Codec.Binary.Base64.String as Base64
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.Char8.Caseless as I
import qualified Data.Map as Map
import Control.Monad (liftM, ap)
import qualified Control.Monad.Fail as Fail
import System.IO.Unsafe
#if !MIN_VERSION_time(1,5,0)
import System.Locale (TimeLocale, defaultTimeLocale, iso8601DateFormat)
#endif
import Prelude -- silence AMP warnings.

-- | A directory is a list of groups of semantically related entities. These
-- entities are grouped together in RFC 2425 using @BEGIN ... END@ pairs.
-- Within a group properties are further grouped together by the property
-- types.
type Directory u = [Map.Map Type [Property u]]

data Property u = Prop
    { prop_type :: Type
    , prop_parameters :: [Parameter]
    , prop_value :: Value u }
                  deriving Show

data Type = Type
    { type_group :: Maybe I.ByteString
    , type_name :: I.ByteString }
            deriving (Eq, Ord, Show)

-- | Make a property type without any grouping.
nakedType :: I.ByteString -> Type
nakedType name = Type { type_group = Nothing, type_name = name }

-- | Check whether the given property is an instance of the given type.
(@@) :: Property u -> I.ByteString -> Bool
prop @@ name = prop_type prop == nakedType name

data Parameter = Param
    { param_name :: I.ByteString
    , param_values :: [B.ByteString] }
                 deriving Show

-- | Find the parameter values for a given parameter name.
lookupParameter :: I.ByteString -> [Parameter] -> Maybe [B.ByteString]
lookupParameter _ [] = Nothing
lookupParameter pname (p:ps)
    | param_name p == pname = Just (param_values p)
    | otherwise = lookupParameter pname ps

type URI = B.ByteString

-- | This is sufficient to represent values whose specification is defined in
-- RFC 2425. Values with other specifications can be represented via the
-- 'IANAValue' constructor.
data Value u = URI URI
             | Text B.ByteString
             | Date Day
             | Time DiffTime
             | DateTime UTCTime
             | Integer Integer
             | Boolean Bool
             | Float Float
-- Decode a list of values as a list of properties, since rfc2425
-- considers them to be semantically equivalent.
--           | List (Value u)
             | IANAValue u -- ^ An IANA defined type not part of rfc2425
               deriving (Eq, Show)

-- | Instantiate Value with this phantom type to indicate that property types
-- should be none other than those defined in rfc2425.
data Rfc2425Value

instance Show Rfc2425Value where
    show _ = undefined

-- | The type of parsers for property values, for instance to read an integer
-- property, text property, etc.
type ValueParser u = (Type, [Parameter]) -> B.ByteString -> [Value u]

-- | Break the input into logical lines, unfolding lines that span multiple
-- physical lines.
unfoldLines :: B.ByteString -> [B.ByteString]
unfoldLines s | B.null s = []
              | otherwise = B.foldr f [B.empty] s where
    f '\r' (xs:xss) | Just (h1, xs') <- B.uncons xs,
                      Just (h2, xs'') <- B.uncons xs' =
                      case (h1, h2) of
                        ('\n', ' ') -> xs'':xss
                        ('\n', '\t') -> xs'':xss
                        ('\n', _) -> "":xs':xss
                        _ -> error "Malformed input: no LF after a CR."
                    | otherwise = "":xss
    f x ~(xs:xss) = B.cons x xs : xss

newtype P a = P { unP :: B.ByteString -> (a, B.ByteString) }

instance Functor P where
    fmap = liftM

instance Applicative P where
    pure x = P $ \s -> (x, s)
    (<*>) = ap

instance Monad P where
    return = pure
    m >>= k = P $ \s -> let (a, s') = unP m s in unP (k a) s'

instance Fail.MonadFail P where
    fail = undefined

pattern :: B.ByteString   -- ^ Text of the regular expression.
  -> P B.ByteString -- ^ The matching part of the input.
pattern pat =
    let Right r = unsafePerformIO $ compile compBlank execAnchored pat
    in P $ \s -> unsafePerformIO $ do
                   Right result <- regexec r s
                   return $ case result of
                              Just (_, match, s', _) -> (match, s')
                              Nothing -> error $ "Parse error: "
                                         ++ take 50 (show (B.unpack s)) ++ " ..."

capture :: B.ByteString     -- ^ Text of the regular expression containing capturing groups.
        -> P [B.ByteString] -- ^ The captured subparts of the input.
capture pat =
    let Right r = unsafePerformIO $ compile compBlank execAnchored pat
    in P $ \s -> unsafePerformIO $ do
                   Right result <- regexec r s
                   return $ case result of
                              Just (_, _, s', captures) -> (captures, s')
                              Nothing -> error $ "Parse error: "
                                         ++ take 50 (show (B.unpack s)) ++ " ..."

-- | Parse one character in the string.
nextChar :: P Char
nextChar = P $ \s -> (B.head s, B.tail s)

-- | Produces a map where properties are grouped together using their type as key.
parseDirectory :: ValueParser u
                -- ^ Given a Property Type and a list of parameters,
                -- parse a string representation into a Value.
                -> B.ByteString
                -> Directory u
parseDirectory valparse = fromList . parseDirectory' valparse

-- | An alternative version of 'parseDirectory' that produces a list
-- of properties rather than a mapping from property types to
-- properties. Note that here properties in the list are in the same
-- order as in the input string.
parseDirectory' :: ValueParser u
               -> B.ByteString
               -> [Property u]
parseDirectory' valparse = concatMap (fst . unP (pa_property valparse)) . unfoldLines

-- | Group properties into blocks delimited by @begin..end@ pairs.
groupByBeginEnd :: [Property u] -> [[Property u]]
groupByBeginEnd [] = []
groupByBeginEnd xs = tail $ foldr f [[]] xs
    where f p (ps:pss) | p @@ "begin" =
                           [] : (p:ps) : pss
          f p (ps:pss) = (p:ps):pss
          f _ _ = error "impossible."

-- | Build a directory from a list of properties.
fromList :: [Property u] -> Directory u
fromList = map (Map.fromListWith (\x y -> x ++ y) . map (\p -> (prop_type p, [p])))
           . groupByBeginEnd

-- | Parse a string representation into a property. Note that the return type
-- here is actually a list of properties, because we desugar properties whose
-- values are lists into a list of properties, one for each element of the
-- value list.
pa_property :: ValueParser u
              -- ^ Given a Property Type and a list of parameters, parse a
              -- string representation into a (list of) Value.
              -> P [Property u]
pa_property valparse = do
  [groupt, typt, sept] <-
      capture "(?U)(?:((?:[[:alnum:]]|-)+).)?((?:[[:alnum:]]|-)+)(:|;)"
  params <- case sept of
              ";" -> pa_parameterList
              ":" -> return []
              _ -> error "pa_property: bad separator."
  rest <- pattern ".*$"
  let group = if B.null groupt then Nothing else Just (I.unsensitize groupt)
  let typ = Type { type_group = group, type_name = I.unsensitize typt }
      mkprop v = Prop { prop_type = typ
                      , prop_parameters = params
                      , prop_value = v }
  return $ map mkprop $ valparse (typ, params) (decodeValue params rest)

pa_parameterList :: P [Parameter]
pa_parameterList = aux where
    paramName  = capture "((?:[[:alnum:]]|-)+)="
    paramValue = capture "(?:([^,;:\"]*)|\"([^\"]*)\")(,?)"
    paramValues = do
      [val,qval,sep] <- paramValue
      vs <- case sep of
              "," -> paramValues
              _ -> return []
      return $ if B.null qval then val:vs else qval:vs
    aux = do [name] <- paramName
             vs <- paramValues
             sep <- nextChar
             ps <- case sep of
                     ';' -> aux
                     ':' -> return []
                     _ -> error "pa_parameterList: bad separator."
             return $ Param { param_name = I.unsensitize name, param_values = vs } : ps

-- | Properties may indicate an encoding, so this decodes the value
-- if need be before parsing.
decodeValue :: [Parameter] -> B.ByteString -> B.ByteString
decodeValue = codec Base64.decode

-- | Properties may indicate an encoding, so this encodes the value if need be
-- after printing.
encodeValue :: [Parameter] -> B.ByteString -> B.ByteString
encodeValue = codec Base64.encode

codec :: (String -> String) -> [Parameter] -> B.ByteString -> B.ByteString
codec f params input =
    case lookupParameter "encoding" params of
      Nothing -> input
      Just ["b"] -> B.pack $ f $ B.unpack input
      Just ["B"] -> B.pack $ f $ B.unpack input
      _ -> error "Unknown encoding."

-- A few canned parsers for value types defined in rfc2425

-- | time-1.4 compat wrapper.
parseTime :: ParseTime t => TimeLocale -> String -> String -> t
#if MIN_VERSION_time(1,5,0)
parseTime = parseTimeOrError True
#else
parseTime = readTime
#endif

pa_URI :: ValueParser u
pa_URI _ = (:[]) . Text

-- | Unescape slashes, newlines and commas.
pa_text :: ValueParser u
pa_text tps = take 1 . pa_textList tps

pa_date :: ValueParser u
pa_date _ =
    (:[]) . Date . parseTime defaultTimeLocale (iso8601DateFormat Nothing) . B.unpack

pa_time :: ValueParser u
pa_time _ =
    (:[]) . Time . utctDayTime . parseTime defaultTimeLocale "%T" . B.unpack

pa_dateTime :: ValueParser u
pa_dateTime _ =
    (:[]) . DateTime .
    parseTime defaultTimeLocale (iso8601DateFormat (Just "T%T")) .
    B.unpack

pa_integer :: ValueParser u
pa_integer _ = (:[]) . Integer . fst . fromJust . B.readInteger

pa_boolean :: ValueParser u
pa_boolean _ "TRUE" = [Boolean True]
pa_boolean _ "FALSE" = [Boolean False]
pa_boolean _ _ = error "Not a valid boolean."

pa_float :: ValueParser u
pa_float _ = (:[]) . Float . read . B.unpack

pa_textList :: ValueParser u
pa_textList _ "" = []
pa_textList _ s = map (Text . B.pack . B.unpack) $ B.foldr f [B.empty] s
    where f ','  (xs:xss) = B.empty : xs : xss
          f '\\' ("":xs:xss) = B.cons ',' xs : xss
          f '\\' (xs:xss) | Just ('n',_)  <- B.uncons xs =
                            B.append "\r\n" xs : xss
          f '\\' (xs:xss) | Just ('N',_)  <- B.uncons xs =
                            B.append "\r\n" xs : xss
          f '\\' (xs:xss) | Just ('\\',_) <- B.uncons xs = B.cons '\\' xs : xss
          f x (xs:xss) = B.cons x xs : xss
          f _ _ = error "impossible"

-- | Take a parser for single values to a parser for a list of values. This
-- assumes that the separator between values is the "," character, and that
-- values do not contain commas themselves.
many :: ValueParser u -> ValueParser u
many pa tps input = map (head . pa tps) $ breakAll input
    where breakAll "" = []
          breakAll xs = ys : breakAll (B.drop 1 zs)
              where (ys, zs) = B.span (/= ',') xs

-- Printing

showBS :: Show a => a -> B.ByteString
showBS = B.pack . show

-- | Escape any occurrence of the characters given as first argument with a
-- backslash. Newlines are always replaced by the two character sequence
-- @"\\n"@. The backslash character is always escaped.
escape :: B.ByteString -> B.ByteString -> B.ByteString
escape chars = B.foldr f "" where
    f '\r' xs | Just ('\n', xs') <- B.uncons xs = B.append "\\n" xs'
              | otherwise = error "CR not followed by LF."
    f x xs | x `B.elem` B.cons '\\' chars = B.cons '\\' (B.cons x xs)
           | otherwise = B.cons x xs

-- Pretty printing of values
class PrintValue a where
    printValue :: a -> B.ByteString

instance PrintValue u => PrintValue (Value u) where
    printValue (URI v) = v
    printValue (Text v) = escape "," $ v
    printValue (Date v) = showBS v
    printValue (Time v) = showBS v
    printValue (DateTime v) = showBS v
    printValue (Integer v) = showBS v
    printValue (Boolean True) = "TRUE"
    printValue (Boolean False) = "FALSE"
    printValue (Float v) = showBS v
    printValue (IANAValue v) = printValue v

instance PrintValue Rfc2425Value where
    printValue _ = error "No other types in RFC 2425."

printDirectory :: PrintValue u => Directory u -> B.ByteString
printDirectory = printDirectory' . concat . concat . map Map.elems

printDirectory' :: PrintValue u => [Property u] -> B.ByteString
printDirectory' props = B.intercalate "\r\n" $ map printProperty props

printProperty :: PrintValue u => Property u -> B.ByteString
printProperty prop =
    if null params
    then B.concat [ printType (prop_type prop), ":"
                  , encodeValue params $ printValue $ prop_value prop ]
    else B.concat [ printType (prop_type prop), ";"
                  , B.concat $ map printParameter $ prop_parameters prop, ":"
                  , printValue $ prop_value prop ]
    where params = prop_parameters prop

printType :: Type -> B.ByteString
printType typ =
    I.sensitize $ case type_group typ of
                    Just group -> I.concat [group, ".", type_name typ]
                    Nothing -> type_name typ

printParameter :: Parameter -> B.ByteString
printParameter param = B.concat [I.sensitize $ param_name param, "="
                                , B.intercalate "," $ param_values param]
