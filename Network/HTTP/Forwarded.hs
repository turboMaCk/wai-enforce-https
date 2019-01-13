{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.Wai.Middleware.EnforceHTTPS
-- Copyright   : (c) Marek Fajkus
-- License     : BSD3
--
-- Maintainer  : marek.faj@gmail.com
--
-- Parsing and Serialization of [Forwarded](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Forwarded)
-- HTTP header values.

module Network.HTTP.Forwarded
  ( Forwarded(..)
  , parseForwarded
  , serializeForwarded
  ) where

import           Data.ByteString      (ByteString)
import           Data.CaseInsensitive (CI)
import           Data.Maybe           (catMaybes)
import           Data.Monoid          ((<>))
import           Data.Word            (Word8)

import qualified Data.ByteString      as ByteString
import qualified Data.CaseInsensitive as CaseInsensitive


-- | Representation of Forwarded header data
-- All field are optional
data Forwarded = Forwarded
  { forwardedBy    :: Maybe ByteString
  , forwardedFor   :: Maybe ByteString
  , forwardedHost  :: Maybe ByteString
  , forwardedProto :: Maybe (CI ByteString)
  } deriving (Eq, Show)


empty :: Forwarded
empty = Forwarded
  { forwardedBy    = Nothing
  , forwardedFor   = Nothing
  , forwardedHost  = Nothing
  , forwardedProto = Nothing
  }


-- | Parse @ByteString@ to Forwarded header
-- Note that this function works with the values
-- of the header only. Extraction of value
-- from header depends what representation of headers
-- you're using.
--
-- In case of Wai you can extract headers as following:
--
-- > :set -XOverloadedStrings
-- > import Network.Wai
-- > import Network.HTTP.Forwarded
-- > getForwarded req = parseForwarded <$> "forwarded" `lookup` requestHeaders req
-- > :t getForwarded
-- > getForwarded :: Request -> Maybe Forwarded
parseForwarded :: ByteString -> Forwarded
parseForwarded = foldr accumulate empty . parseForwarded'
  where
    accumulate (key, val) acc =
      case key of
        "by"    -> acc { forwardedBy = Just val }
        "for"   -> acc { forwardedFor = Just val }
        "host"  -> acc { forwardedHost = Just val }
        "proto" -> acc { forwardedProto = Just $ CaseInsensitive.mk val }
        _       -> acc


parseForwarded' :: ByteString -> [ (ByteString, ByteString) ]
parseForwarded' s
  | ByteString.null s = []
  | otherwise =
      let (x, y) = breakDiscard 59 s -- semicolon
      in parsePart x : parseForwarded' y


parsePart :: ByteString -> (ByteString, ByteString)
parsePart s = (key', value)
  where
    (key, value) =
      breakDiscard 61 s -- equals sign
    key' =
      ByteString.dropWhile (== 32) key -- space


breakDiscard :: Word8 -> ByteString -> (ByteString, ByteString)
breakDiscard w s = (x, ByteString.drop 1 y)
  where
    (x, y) =
      ByteString.break (== w) s


-- | Serialize `Forwarded` data type back
-- to ByteString representation.
serializeForwarded :: Forwarded -> ByteString
serializeForwarded Forwarded { .. } =
    ByteString.intercalate "; " $ catMaybes xs
    where
      xs =
        [ strVal "by" forwardedBy
        , strVal "for" forwardedFor
        , strVal "host" forwardedHost
        , strVal "proto" $ CaseInsensitive.original <$> forwardedProto
        ]

      strVal _ Nothing      = Nothing
      strVal key (Just val) = Just $ key <> "=" <> val
