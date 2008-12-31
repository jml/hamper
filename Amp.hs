{-# LANGUAGE TypeSynonymInstances #-}

module Amp
    (
      AmpBox
    , Bytes
    , ampFunction
    , box
    , connectTCP
    , disconnect
    , textToBytes
    , unbox
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Binary
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as UTF8
import Network.Socket
import System.IO
import Util


type Bytes = [Word8]

newtype AmpKey = AmpKey Bytes deriving (Ord, Eq)

newtype AmpValue = AmpValue Bytes

newtype AmpBox = AmpBox (Map.Map AmpKey AmpValue)
_unAmpBox (AmpBox box) = box


-- Convert between text and UTF8-encoded bytes.
textToBytes = UTF8.encode
u = textToBytes
bytesToText = UTF8.decode


ampKey :: String -> AmpKey
ampKey = (AmpKey . textToBytes)

unAmpKey :: AmpKey -> String
unAmpKey (AmpKey x) =  bytesToText x

ampValue :: String -> AmpValue
ampValue = (AmpValue . textToBytes)

unAmpValue :: AmpValue -> String
unAmpValue (AmpValue x) = bytesToText x


-- AMP Constants
_ASK = ampKey "_ask"
_ANSWER = ampKey "_answer"
_COMMAND = ampKey "_command"
_ERROR = u "_error"
_ERROR_CODE = u "_error_code"
_ERROR_DESCRIPTION = u "_error_description"
_UNKNOWN_ERROR_CODE = u "UNKNOWN"
_UNHANDLED_ERROR_CODE = u "UNHANDLED"


instance Binary AmpKey where
    put (AmpKey bytes) = do put (fromIntegral (length bytes) :: Word16)
                            mapM_ put bytes

    get = do numBytes <- get :: Get Word16
             payload <- replicateM (fromIntegral numBytes) getWord8
             return (AmpKey payload)


instance Binary AmpValue where
    put (AmpValue bytes) = do put (fromIntegral (length bytes) :: Word16)
                              mapM_ put bytes

    get = do numBytes <- get :: Get Word16
             payload <- replicateM (fromIntegral numBytes) getWord8
             return (AmpValue payload)


instance Binary AmpBox where
    put (AmpBox box) = mapM_ putAmpKeyPair (Map.toList box)
      where
        putAmpKeyPair (key, value) = do put key
                                        put value

    get = do key <- get :: Get AmpKey
             value <- get :: Get AmpValue
             return (AmpBox (Map.fromList [(key, value)]))


instance Show AmpBox where
    show box = concat ["AmpBox(", (showMap . unboxMap) box, ")"]


_addToBox key value box = (AmpBox . Map.insert key value) (_unAmpBox box)
makeBoxCommand command False box = _addToBox _COMMAND command box
makeBoxCommand command True box =
    _addToBox _ASK (ampValue "unique!") (makeBoxCommand command False box)


-- Because it's a pain to play with AmpBoxes in the interpreter, these helpers
-- convert from [(String, String)] -> AmpBox and back.

boxMap = AmpBox . (transformMap ampKey ampValue)
unboxMap = (transformMap unAmpKey unAmpValue) . _unAmpBox

box = boxMap . Map.fromList
unbox = Map.toList . unboxMap


-- An AmpSession represents a session between two AMP peers. It exists only
-- for the lifetime of the connection.
--
-- An AmpSession tracks the answers we've received from the peer. It also
-- knows how to create tags to identify new question/answers. (Eventually, it
-- will also have a registry of responders to commands).

newtype Tag = Tag Integer

data AmpSession = AmpSession {
      replies :: (MVar (Map.Map AmpValue (MVar AmpBox))),
      lastTag :: MVar Tag
    }


newAmpSession :: IO AmpSession
newAmpSession = liftM2 AmpSession (newMVar Map.empty) (newMVar (Tag 0))


getNextTag :: AmpSession -> IO Tag
getNextTag ampSession =
  modifyMVar (lastTag ampSession) incrementTag
  where incrementTag (Tag n) = return (Tag (n + 1), Tag n)


connectTCP :: HostName -> String -> IO Handle
connectTCP hostname port =
    do addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Mark the socket for keep-alive handling since it may be idle
       -- for long periods of time
       setSocketOption sock KeepAlive 1

       -- Connect to server
       connect sock (addrAddress serveraddr)

       -- Make a Handle out of it for convenience
       h <- socketToHandle sock ReadWriteMode

       -- We're going to set buffering to BlockBuffering and then
       -- explicitly call hFlush after each message, below, so that
       -- messages get logged immediately
       hSetBuffering h (BlockBuffering Nothing)

       return h


sendAMPMessage :: Handle -> AmpBox -> IO ()
sendAMPMessage handle box = do
  B.hPut handle (encode box)
  hFlush handle

getAMPMessage :: Handle -> IO AmpBox
getAMPMessage handle = do
  contents <- B.hGetContents handle
  return $ decode contents


callAMP :: Handle -> AmpBox -> IO AmpBox
callAMP handle box = do
  sendAMPMessage handle box
  getAMPMessage handle


ampFunction :: Handle -> String -> AmpBox -> IO AmpBox
ampFunction handle command arguments =
    let box = makeBoxCommand (ampValue command) True arguments in
    do reply <- callAMP handle box
       (return . AmpBox . (Map.delete _ANSWER) . _unAmpBox) reply


disconnect :: Handle -> IO ()
disconnect = hClose


-- AMP allows for a multitude of arguments.
class Argument a where
    toAmpValue :: a -> AmpValue
    fromAmpValue :: AmpValue -> a


instance Argument Integer where
    toAmpValue = (ampValue . show)
    fromAmpValue value = (read . unAmpValue) value :: Integer


instance Argument Bytes where
    toAmpValue = AmpValue
    fromAmpValue (AmpValue bytes) = bytes


instance Argument String where
    toAmpValue = ampValue
    fromAmpValue = unAmpValue


instance Argument Tag where
    toAmpValue (Tag tag) = toAmpValue tag
    fromAmpValue value = Tag ((read . unAmpValue) value :: Integer)
