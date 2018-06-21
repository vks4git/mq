{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.MQ.Protocol.Technical
  (
    KillConfig (..)
  , MonitoringData (..)
  ) where

import           Data.Aeson                   (FromJSON (..), ToJSON (..),
                                               genericParseJSON, genericToJSON)
import           Data.Aeson.Casing            (aesonPrefix, snakeCase)
import           Data.MessagePack.Types.Class (MessagePack (..))
import           GHC.Generics                 (Generic)
import           System.MQ.Protocol           (Id, MessageType (..), Timestamp)
import           System.MQ.Protocol.Class     (MessageLike (..), Props (..))

--------------------------------------------------------------------------------
-- | Configuration for kill task
--
newtype KillConfig = KillConfig { killTaskId :: Id
                                }
  deriving (Eq, Show, Generic)

instance ToJSON KillConfig where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON KillConfig where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance MessagePack KillConfig

instance MessageLike KillConfig where
  props = Props "kill" Config

--------------------------------------------------------------------------------
-- | Format of data that is produced as result of monitoring task
--
data MonitoringData = MonitoringData { mSyncTime :: Timestamp
                                     , mName     :: String
                                     , mIsAlive  :: Bool
                                     , mMessage  :: String
                                     }
  deriving (Eq, Show, Generic)

instance ToJSON MonitoringData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MonitoringData where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance MessagePack MonitoringData

instance MessageLike MonitoringData where
  props = Props "monitoring" Data
