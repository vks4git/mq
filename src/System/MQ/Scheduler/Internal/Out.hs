{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.Out
  (
    runSchedulerOut
  ) where

import           Control.Concurrent                  (forkIO)
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (foreverSafe, runMQMonad)
import           System.MQ.Scheduler.Internal.Config (NetConfig (..),
                                                      SchedulerCfg, comHostPort,
                                                      techHostPort)
import           System.MQ.Transport                 (BindTo (..),
                                                      HostPort (..), anyHost,
                                                      contextM)
import           System.MQ.Transport.ByteString      (pub, pull)

-- | SchedulerIn receives messages from the SchedulerLogic and translates them into the "world".
--
runSchedulerOut :: NetConfig -> IO ()
runSchedulerOut NetConfig{..} = do
    infoM name "start working..."
    _ <- forkIO $ processing techHostPort
    processing comHostPort

  where
    name :: String
    name = "SchedulerOut"

    processing :: (SchedulerCfg -> HostPort) -> IO ()
    processing hostPortSelector = runMQMonad $ do
        context'        <- contextM
        let fromLogicHP = HostPort anyHost (port . hostPortSelector $ schedulerLogicOut)
        fromLogic       <- bindTo fromLogicHP context'
        toWorld         <- bindTo (hostPortSelector schedulerOut) context'
        foreverSafe name (pull fromLogic >>= pub toWorld)
