{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.In
  (
    runSchedulerIn
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
import           System.MQ.Transport.ByteString      (pull, push)

-- | SchedulerIn receives messages from the @world@ and translates them into SchedulerLogic.
--
runSchedulerIn :: NetConfig -> IO ()
runSchedulerIn NetConfig{..} = do
    infoM name "start working..."
    _ <- forkIO $ processing techHostPort
    processing comHostPort

  where
    name :: String
    name = "SchedulerIn"

    processing :: (SchedulerCfg -> HostPort) -> IO ()
    processing hostPortSelector = runMQMonad $ do
        context' <- contextM
        let toLogicHP = HostPort anyHost (port . hostPortSelector $ schedulerInLogic)
        fromWorld <- bindTo (hostPortSelector schedulerIn) context'
        toLogic <- bindTo toLogicHP context'
        foreverSafe name (pull fromWorld >>= push toLogic)
