module System.MQ.Transport
  (
    module System.MQ.Transport.Internal.Types
  , push
  , pull
  , pub
  , sub
  ) where

import           System.MQ.Encoding.MessagePack         (pack, unpackM)
import           System.MQ.Monad                        (MQMonad)
import           System.MQ.Protocol                     (Message, MessageTag,
                                                         messageTag)
import           System.MQ.Protocol.Internal.Instances  ()
import qualified System.MQ.Transport.ByteString         as TBS (pub, pull, push,
                                                                sub)
import           System.MQ.Transport.Internal.Instances ()
import           System.MQ.Transport.Internal.Types

-- | Pushes @(tag, content)@ to the 'PushChannel'.
-- @tag::'MessageTag'@ is generated automatically from @content@.
--
push :: PushChannel -> Message -> MQMonad ()
push channel content = let tag' = pack . messageTag $ content
                       in TBS.push channel (tag', pack content)

-- | Pulls @(tag, content)@ from the 'PullChannel'.
--
pull :: PullChannel -> MQMonad (MessageTag, Message)
pull channel = do
  (tag, content) <- TBS.pull channel
  utag           <- unpackM tag
  ucontent       <- unpackM content
  pure (utag, ucontent)

-- | Publishes @(tag, content)@ to the 'PubChannel'.
-- @tag::'MessageTag'@ is generated automatically from @content@.
--
pub :: PubChannel -> Message -> MQMonad ()
pub channel content = let tag' = pack . messageTag $ content
                      in TBS.pub channel (tag', pack content)

-- | Subscribes and gets @(tag, content)@ from the 'SubChannel'.
--
sub :: SubChannel -> MQMonad (MessageTag, Message)
sub channel = do
  (tag, content) <- TBS.sub channel
  utag           <- unpackM tag
  ucontent       <- unpackM content
  pure (utag, ucontent)


