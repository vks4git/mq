{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.MQ.Scheduler.Internal.Config
  (
    NetConfig (..)
  , LogicConfig (..)
  , SchedulerCfg (..)
  , loadLogicConfig
  , loadNetConfig
  , comHostPort
  , techHostPort
  ) where

import           Data.Aeson          (FromJSON (..))
import           Data.Aeson.Picker   ((|--))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           System.BCD.Config   (getConfigText)
import           System.MQ.Transport (Host, HostPort (..), Port)

-- | Contains scheduler logic configuration.
--
newtype LogicConfig = LogicConfig { allowMessages :: [Text] }

-- | Loads scheduler logic configuration from config.json.
--
loadLogicConfig :: IO LogicConfig
loadLogicConfig = do
    config             <- getConfigText
    let getField field = config |-- ["params", "scheduler-logic", field]
    pure $ LogicConfig (getField "allow-messages")

-- | Contains all information for schedulers connections.
--
data NetConfig = NetConfig { schedulerIn       :: SchedulerCfg
                           , schedulerInLogic  :: SchedulerCfg
                           , schedulerLogicOut :: SchedulerCfg
                           , schedulerOut      :: SchedulerCfg
                           }

-- | Loads 'NetConfig' from config.json.
--
loadNetConfig :: IO NetConfig
loadNetConfig = do
    config <- getConfigText
    let getField f = config |-- ["deploy", "monique", f]
    pure $ NetConfig (getField "scheduler-in")
                     (getField "scheduler-in-logic")
                     (getField "scheduler-logic-out")
                     (getField "scheduler-out")

-- | Contains information about host, communication and technical ports.
--
data SchedulerCfg = SchedulerCfg { host     :: Host
                                 , comport  :: Port
                                 , techport :: Port
                                 }
  deriving (Generic)

instance FromJSON SchedulerCfg

-- | Takes 'Host' and communication 'Port' from 'SchedulerCfg'.
--
comHostPort :: SchedulerCfg -> HostPort
comHostPort SchedulerCfg {..} = HostPort host comport

-- | Takes 'Host' and technical 'Port' from 'SchedulerCfg'.
--
techHostPort :: SchedulerCfg -> HostPort
techHostPort SchedulerCfg {..} = HostPort host techport
