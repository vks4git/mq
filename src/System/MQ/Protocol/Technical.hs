{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.MQ.Protocol.Technical
  (
    KillConfig (..)
  , MonitoringData (..)
  ) where

import           Data.Aeson               (FromJSON (..), ToJSON (..),
                                           genericParseJSON, genericToJSON,
                                           object, withObject, (.:), (.=))
import           Data.Aeson.Casing        (aesonPrefix, snakeCase)
import qualified Data.ByteString.Char8    as BSLC8 (pack, unpack)
import           GHC.Generics             (Generic)
import           System.MQ.Encoding.JSON  as JSON (pack, unpack)
import           System.MQ.Protocol       (Hash, MessageType (..), Timestamp,
                                           jsonEncoding)
import           System.MQ.Protocol.Class (MessageLike (..), Props (..))

-- | Configuration for kill task
--
newtype KillConfig = KillConfig { killTaskId :: Hash }
  deriving (Eq, Show, Generic)

instance ToJSON KillConfig where
  toJSON p = object [ "task_id" .= BSLC8.unpack (killTaskId p) ]

instance FromJSON KillConfig where
  parseJSON = withObject "Kill Config" $ \o -> KillConfig . BSLC8.pack <$> o .: "task_id"

instance MessageLike KillConfig where
  props = Props "kill" Config jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack

-- | Format of data that is produced as result of monitoring task
--
data MonitoringData = MonitoringData { mSyncTime  :: Timestamp
                                     , mName      :: String
                                     , mHost      :: String
                                     , mIsRunning :: Bool
                                     , mMessage   :: String
                                     }
  deriving (Eq, Show, Generic)

instance ToJSON MonitoringData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MonitoringData where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance MessageLike MonitoringData where
  props = Props "monitoring" Data jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack
