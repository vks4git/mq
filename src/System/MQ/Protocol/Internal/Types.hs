{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module System.MQ.Protocol.Internal.Types
  (
    Timestamp
  , Id
  , Creator
  , Encoding
  , Spec
  , MessageTag
  , Message (..)
  , MessageType (..)
  , Dictionary (..)
  ) where

import           Data.ByteString               as BS (ByteString)
import           Data.Map.Strict               (Map)
import           Data.MessagePack              ()
import           Data.MessagePack.Types.Object (Object)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic (..))

-- | Dictionary class describes objects that can be turned into an association list (key := value) Object
--
class Dictionary a where
  toDictionary :: a -> Map Text Object
  fromDictionary :: Monad m => Map Text Object -> m a

-- | Represents Unix epoch time in milliseconds.
--
type Timestamp  = Int

-- | Type for identificators
--
type Id         = Text

-- | Message tag is a 'ByteString' with five separated with ':' fields: message type; spec; id; pid; creator.
-- It can be created from 'Message' automatically.
-- See doc/PROTOCOL.md for more details.
--
type MessageTag = Text

-- | Creator is identifier for user or system id that creates message.
--
type Creator    = Text

-- | Encoding is message encoding type. For this moment it can be "JSON" or "MessagePack".
--
type Encoding   = Text

-- | Spec is message specification.
--
type Spec       = Text

-- | 'Message' is the main entity in MQ: various components, controllers and the Scheduler communicate with each other using 'Message's.
--
data Message = Message { msgId        :: Id          -- ^ str format family
                       , msgPid       :: Id          -- ^ str format family
                       , msgCreator   :: Creator     -- ^ str format family
                       , msgCreatedAt :: Timestamp   -- ^ int format family
                       , msgExpiresAt :: Timestamp   -- ^ int format family
                       , msgSpec      :: Spec        -- ^ str format family
                       , msgEncoding  :: Encoding    -- ^ str format family
                       , msgType      :: MessageType -- ^ str format family
                       , msgData      :: ByteString  -- ^ bin format family
                       }
  deriving (Eq, Show, Read, Generic)

-- | 'MessageType' describes valid message types in Monique.
--
data MessageType = Config | Result | Error | Data
  deriving (Eq, Generic)

instance Show MessageType where
  show Config = "config"
  show Result = "result"
  show Error  = "error"
  show Data   = "data"

instance Read MessageType where
  readsPrec _ "config" = [(Config, "")]
  readsPrec _ "result" = [(Result, "")]
  readsPrec _ "error"  = [(Error, "")]
  readsPrec _ "data"   = [(Data, "")]
  readsPrec _ _        = []
