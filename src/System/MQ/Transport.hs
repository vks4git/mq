module System.MQ.Transport
  (
    module System.MQ.Transport.Internal.Types
  , push
  , pull
  , pub
  , sub
  ) where

import           System.MQ.Encoding.Pack                (pack, unpackM)
import           System.MQ.Monad                        (MQMonadS)
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
push :: PushChannel -> Message -> MQMonadS s ()
push channel content = TBS.push channel (messageTag content, pack content)

-- | Pulls @(tag, content)@ from the 'PullChannel'.
--
pull :: PullChannel -> MQMonadS s (MessageTag, Message)
pull channel = do
  (tag, content) <- TBS.pull channel
  ucontent       <- unpackM content
  pure (tag, ucontent)

-- | Publishes @(tag, content)@ to the 'PubChannel'.
-- @tag::'MessageTag'@ is generated automatically from @content@.
--
pub :: PubChannel -> Message -> MQMonadS s ()
pub channel content = TBS.pub channel (messageTag content, pack content)

-- | Subscribes and gets @(tag, content)@ from the 'SubChannel'.
--
sub :: SubChannel -> MQMonadS s (MessageTag, Message)
sub channel = do
  (tag, content) <- TBS.sub channel
  ucontent       <- unpackM content
  pure (tag, ucontent)


