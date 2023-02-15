{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bytecode.Types where

import Data.Serialize
import qualified Data.Text.Encoding as Text
import RIO
import qualified RIO.ByteString as BSC

newtype Txt = Txt Text
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

instance Serialize Txt where
  put (Txt s) = do
    putByteString (Text.encodeUtf8 s)
  get = Txt . Text.decodeUtf8 . BSC.pack <$> tillNull

null :: Put
null = putWord8 0

tillNull :: Get [Word8]
tillNull = do
  b <- getWord8
  if b == 0
    then pure []
    else (b :) <$> tillNull
