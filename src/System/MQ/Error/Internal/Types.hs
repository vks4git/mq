{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}

module System.MQ.Error.Internal.Types
  ( MQError (..)
  , errorProtocol
  , errorEncoding
  , errorTransport
  , errorTag
  , errorTechnical
  , errorKilled
  , errorComponent
  , errorForeign
  , errorIncorrectInput
  ) where

import           GHC.Generics      (Generic)
import           Control.Exception (Exception (..), SomeException (..), toException)
import           Text.Printf       (printf)


-- | MQError represents:
--   * data for the message with type @error@: for more details see file System.MQ.Error.Internal.Instances;
--   * standart error for the 'MQMonad'.
--
data MQError = MQError { errorCode    :: Int
                       , errorMessage :: String
                       }
  deriving (Eq, Generic)

instance Exception MQError where
  toException :: MQError -> SomeException
  toException = SomeException

  fromException :: SomeException -> Maybe MQError
  fromException (SomeException e) = res
    where
      msg = show e
      codeStartIdx = 14
      (err, code) = splitAt codeStartIdx msg
      res = if err == "MQError (code "
              then Just $ MQError (read (take 3 code) :: Int) msg
              else Nothing


instance Show MQError where
  show (MQError c m) = printf "MQError (code %d): %s" c m


--------------------------------------------------------------------------------
-- PROTOCOL ERROR: 1xx
--------------------------------------------------------------------------------

-- | General error in protocol.
errorProtocol :: Int
errorProtocol = 100

-- | Error while encode or decode message.
errorEncoding :: Int
errorEncoding = 101

--------------------------------------------------------------------------------
-- TRANSPORT ERROR: 2xx
--------------------------------------------------------------------------------

-- | General transport error.
errorTransport :: Int
errorTransport = 200

-- | Tag is not valid.
errorTag :: Int
errorTag = 201

--------------------------------------------------------------------------------
-- TECHNICAL ERROR: 3xx
--------------------------------------------------------------------------------

-- | General error in technical layer.
errorTechnical :: Int
errorTechnical = 300

-- | This error shows that message with task was killed intentionally.
errorKilled :: Int
errorKilled = 301

--------------------------------------------------------------------------------
-- COMPONENT ERROR: 5xx
--------------------------------------------------------------------------------

-- | General error that can occured in every component.
errorComponent :: Int
errorComponent = 500

-- | This error occured when one component could not get actual and valid asnwer from another component.
errorForeign :: Int
errorForeign = 501

-- | Received message was valid by structure but not valid in meaning.
errorIncorrectInput :: Int
errorIncorrectInput = 502
