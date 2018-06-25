{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module System.MQ.Protocol.Internal.Types
  (
    Timestamp
  , Id
  , Creator
  , Spec
  , Encrypted
  , Signature
  , MessageTag
  , Message (..)
  , MessageType (..)
  , Secure (..)
  ) where

import           Data.ByteString  as BS (ByteString)
import           Data.MessagePack ()
import           Data.Text        (Text)
import           GHC.Generics     (Generic (..))


-- | 'Timestamp' represents Unix epoch time in milliseconds
--
type Timestamp  = Int

-- | 'Id' represent identificator for the message
--
type Id         = Text

-- | 'MessageTag' is a 'Text' with five separated with ':' fields: message type; spec; id; pid; creator.
-- It can be created from 'Message' automatically.
-- See doc/PROTOCOL.md for more details
--
type MessageTag = Text

-- | 'Creator' is identifier for user or system that creates message
--
type Creator    = Text

-- | 'Spec' is message specification
--
type Spec       = Text

-- | 'Encrypted' is flag to represent that data of the message is encrypted or not
--
type Encrypted  = Bool

-- | 'Signature' is used for verification that message sent from trusted client or service
--
type Signature  = ByteString

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

--------------------------------------------------------------------------------

-- | 'Message' is the main entity in MQ: various components, controllers and the Scheduler communicate with each other using 'Message's.
--
data Message = Message { msgId        :: Id          -- ^ str format family
                       , msgPid       :: Id          -- ^ str format family
                       , msgCreator   :: Creator     -- ^ str format family
                       , msgCreatedAt :: Timestamp   -- ^ int format family
                       , msgExpiresAt :: Timestamp   -- ^ int format family
                       , msgSpec      :: Spec        -- ^ str format family
                       , msgType      :: MessageType -- ^ str format family
                       , msgData      :: ByteString  -- ^ bin format family
                       , msgEncrypted :: Encrypted   -- ^ bool format family
                       , msgSignature :: Signature   -- ^ bin format family
                       }
  deriving (Eq, Show, Read, Generic)

--------------------------------------------------------------------------------

type PrivateKey = ByteString

type PublicKey = ByteString

data Secure = NotSecured
            | Signed PrivateKey
            | Encrypted PublicKey
            | SignedAndEncrypted PrivateKey PublicKey
