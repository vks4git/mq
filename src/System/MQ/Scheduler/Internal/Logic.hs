{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.Logic
  (
    runSchedulerLogic
  ) where

import           Control.Concurrent                  (forkIO)
import           Data.ByteString                     (ByteString)
import           Data.String                         (IsString (..))
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (MQMonad, foreverSafe,
                                                      runMQMonad)
import           System.MQ.Protocol                  (messageSpec)
import           System.MQ.Scheduler.Internal.Config (LogicConfig (..),
                                                      NetConfig (..),
                                                      comHostPort, techHostPort)
import           System.MQ.Transport                 (ConnectTo (..),
                                                      PullChannel, PushChannel,
                                                      contextM)
import           System.MQ.Transport.ByteString      (pull, push)

-- | SchedulerLogic receives messages from SchedulerIn, make some logic and (maybe) sends it to SchedulerOut.
-- For this moment the logic is:
-- * filter messages by spec field: if message spec not listed in config.json, in would not be passed.
--
runSchedulerLogic :: NetConfig -> LogicConfig -> IO ()
runSchedulerLogic NetConfig{..} LogicConfig{..} = do
    infoM name "start working..."
    _ <- forkIO processingTech
    processingCom allowList

  where
    allowList :: [ByteString]
    allowList = fromString <$> allowMessages

    name :: String
    name = "SchedulerLogic"

    processingCom :: [ByteString] -> IO ()
    processingCom allowList' = runMQMonad $ do
        context'         <- contextM
        fromInToLogic    <- connectTo (comHostPort schedulerInLogic) context'
        fromLogictoWorld <- connectTo (comHostPort schedulerLogicOut) context'
        foreverSafe name $ comLogic allowList' fromInToLogic fromLogictoWorld

    -- | Function with scheduler logic.
    comLogic :: [ByteString] -> PullChannel -> PushChannel -> MQMonad ()
    -- if list of allow messages is empty then send every message further
    --
    comLogic [] fromIn toOut = pull fromIn >>= push toOut
    -- else only messages with spec from @allowList@ are send further
    --
    comLogic allowList' fromIn toOut = do
        m@(tag, _) <- pull fromIn
        if messageSpec tag `elem` allowList'
        then push toOut m
        else pure ()


    processingTech :: IO ()
    processingTech = runMQMonad $ do
        context' <- contextM
        fromInToLogic    <- connectTo (techHostPort schedulerInLogic) context'
        fromLogictoWorld <- connectTo (techHostPort schedulerLogicOut) context'
        foreverSafe name (pull fromInToLogic >>= push fromLogictoWorld)


