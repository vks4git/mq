{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.MQ.Scheduler.Internal.Logic
  (
    runSchedulerLogic
  ) where

import           Control.Concurrent                  (forkIO)
import           Control.Monad                       (when)
import           Data.String                         (IsString (..))
import           System.Log.Logger                   (infoM)
import           System.MQ.Error                     (MQError (..), errorKilled)
import           System.MQ.Monad                     (MQMonad, foreverSafe,
                                                      runMQMonad)
import           System.MQ.Protocol                  (Message (..),
                                                      MessageLike (..),
                                                      Props (..), createMessage,
                                                      messageSpec)
import           System.MQ.Protocol.Technical        (KillConfig (..))
import           System.MQ.Scheduler.Internal.Config (LogicConfig (..),
                                                      NetConfig (..),
                                                      comHostPort, techHostPort)
import           System.MQ.Transport                 (ConnectTo (..),
                                                      PullChannel, PushChannel,
                                                      contextM)
import qualified System.MQ.Transport                 as T (pull, push)
import           System.MQ.Transport.ByteString      (pull, push)

-- | SchedulerLogic receives messages from SchedulerIn, make some logic and (maybe) sends it to SchedulerOut.
-- For this moment the logic is:
-- * filter messages by spec field: if message spec not listed in config.json, in would not be passed.
--
runSchedulerLogic :: NetConfig -> LogicConfig -> IO ()
runSchedulerLogic NetConfig{..} LogicConfig{..} = do
    infoM name "start working..."
    _ <- forkIO processingTech
    processingCom

  where

    name :: String
    name = "SchedulerLogic"

    processingCom :: IO ()
    processingCom  = runMQMonad $ do
        context'         <- contextM
        fromInToLogic    <- connectTo (comHostPort schedulerInLogic) context'
        fromLogictoWorld <- connectTo (comHostPort schedulerLogicOut) context'
        foreverSafe name $ comLogic allowMessages fromInToLogic fromLogictoWorld

    -- | Function with scheduler logic.
    comLogic :: [String] -> PullChannel -> PushChannel -> MQMonad ()
    -- if list of allow messages is empty then send every message further
    --
    comLogic [] fromIn toOut = pull fromIn >>= push toOut
    -- else only messages with spec from @allowList@ are send further
    --
    comLogic allowList fromIn toOut = do
        m@(tag, _) <- pull fromIn
        if messageSpec tag `elem` allowList
        then push toOut m
        else pure ()

    processingTech :: IO ()
    processingTech = runMQMonad $ do
        context' <- contextM
        fromInToLogic    <- connectTo (techHostPort schedulerInLogic) context'
        fromLogicToWorld <- connectTo (techHostPort schedulerLogicOut) context'
        toSchedulerOut   <- connectTo (comHostPort schedulerLogicOut) context'
        foreverSafe name $ do
          (tag, msg) <- T.pull fromInToLogic
          when (messageSpec tag == fromString (spec (props :: Props KillConfig))) $ processKillMsg toSchedulerOut msg
          T.push fromLogicToWorld msg

    processKillMsg :: PushChannel -> Message -> MQMonad ()
    processKillMsg toSchedulerOut Message{..} = do
        killId <- killTaskId <$> unpackM msgData
        newMsg <- createMessage killId msgCreator msgExpiresAt (MQError errorKilled "task killed")
        T.push toSchedulerOut newMsg
