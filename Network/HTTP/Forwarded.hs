{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.HTTP.Forwarded (Forwarded(..), parseForwarded, encodeForwarded) where

import           Data.ByteString      (ByteString)
import           Data.CaseInsensitive (CI)
import           Data.Maybe           (catMaybes)
import           Data.Word            (Word8)

import qualified Data.ByteString      as ByteString
import qualified Data.CaseInsensitive as CaseInsensitive


data Forwarded = Forwarded
  { forwardedBy    :: Maybe ByteString
  , forwardedFor   :: Maybe ByteString
  , forwardedHost  :: Maybe ByteString
  ,  forwardedProto :: Maybe (CI ByteString)
  } deriving(Eq)


instance Show Forwarded where
  show = show . ("Forwarded: " <>) <$> encodeForwarded


empty :: Forwarded
empty = Forwarded
  { forwardedBy    = Nothing
  , forwardedFor   = Nothing
  , forwardedHost  = Nothing
  , forwardedProto = Nothing
  }


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


encodeForwarded :: Forwarded -> ByteString
encodeForwarded Forwarded { .. } =
    join "; " (catMaybes xs)
    where
      join by = foldr (join' by) ""

      join' by part str
        | str == "" = part
        | otherwise = part <> by <> str

      xs =
        [ strVal "by" forwardedBy
        , strVal "for" forwardedFor
        , strVal "host" forwardedHost
        , strVal "proto" $ CaseInsensitive.original <$> forwardedProto
        ]

      strVal _ Nothing      = Nothing
      strVal key (Just val) = Just $ key <> "=" <> val
