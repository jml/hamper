-- AMP implementation.
-- See twisted.protocols.amp.

import Control.Monad
import Data.Binary
import Data.Bits
import Data.Word
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Codec.Binary.UTF8.String as UTF8


double f (x, y) = (f(x), f(y))

toFlatList ((x, y) : xs) = x : y : toFlatList xs
toFlatList [] = []

fromFlatList :: (Ord a) => [a] -> [(a, a)]
fromFlatList (x : y : xs) = (x, y) : fromFlatList xs
fromFlatList [] = []

mapToFlat = toFlatList . Map.toList

flatToMap :: (Ord a) => [a] -> (Map.Map a a)
flatToMap = Map.fromList . fromFlatList


-- Convert between text and UTF8-encoded bytes.

textToBytes = UTF8.encode
bytesToText = UTF8.decode

_ASK = textToBytes "_ask"
_ANSWER = textToBytes "_answer"
_COMMAND = textToBytes "_command"
_ERROR = textToBytes "_error"
_ERROR_CODE = textToBytes "_error_code"
_ERROR_DESCRIPTION = textToBytes "_error_description"
_UNKNOWN_ERROR_CODE = textToBytes "UNKNOWN"
_UNHANDLED_ERROR_CODE = textToBytes "UNHANDLED"


-- An AMP box is a packet in the AMP protocol, that made up of key:value pairs
-- (where both key and value are byte strings). On the wire, a box looks like
-- a list of length-prefixed values, alternating between key and value,
-- terminated by an empty key (i.e. two null bytes.)

newtype AmpBox = AmpBox (Map.Map [Word8] [Word8])
unAmpBox (AmpBox box) = box

max_key_length = 0xff
max_value_length = 0xffff

packLength n = (B.unpack . encode) (fromIntegral n :: Word16)

serializeBoxBytes bytes = (packLength (length bytes)) ++ bytes
serializeBoxKey = serializeBoxBytes
serializeBoxValue = serializeBoxBytes
serializeBoxPair (key, value) =
    (serializeBoxKey key) ++ (serializeBoxValue value)

boxTerminator = serializeBoxKey []

serializeBox :: AmpBox -> [Word8]
serializeBox (AmpBox box) =
    concat (map serializeBoxPair (Map.toList box)) ++ boxTerminator


-- Because it's a pain to play with AmpBoxes in the interpreter, these helpers
-- convert from [(String, String)] -> AmpBox and back.
box x = (AmpBox . Map.fromList) (map (double textToBytes) x)
unbox (AmpBox x) = map (double bytesToText) (Map.toList x)


-- Commands
_addToBox key value box = AmpBox (Map.insert key value (unAmpBox box))
makeBoxCommand command False box = _addToBox _COMMAND command box
makeBoxCommand command True box =
    _addToBox _ASK (textToBytes "unique!") (makeBoxCommand command False box)

add a b = makeBoxCommand (textToBytes "Sum") True (box [("a", (show a)), ("b", (show b))])

-- Need mapping from String (parameter name) to ParameterType
-- Need mapping from String to Parameter*
-- Take both of these, and convert to a box
-- Take the box and serialize it (maybe with additional AMP pragma)


data AmpByteString = AmpByteString [Word8]
instance Binary AmpByteString where
    put (AmpByteString bytes) = do put (fromIntegral (length bytes) :: Word16)
                                   mapM_ put bytes
    get = do numBytes <- get :: Get Word16
             do payload <- replicateM (fromIntegral numBytes) getWord8
                return (AmpByteString payload)

unAmpByteString (AmpByteString string) = string

data AmpString = AmpString String
unAmpString (AmpString string) = string


ampTextToBytes = AmpByteString . textToBytes . unAmpString
ampBytesToText = AmpString . bytesToText . unAmpByteString

instance Binary AmpString where
    put = put . ampTextToBytes
    get = fmap ampBytesToText get

encodeAmpByteString = encode . AmpByteString . textToBytes

doUntil predicate action =
    do result <- action
       if predicate result
          then return [result]
          else do results <- (doUntil predicate action)
                  return (result : results)

doWhile predicate action = doUntil (not. predicate) action


instance Binary AmpBox where
    put (AmpBox box) = do mapM_ (put . AmpByteString) (mapToFlat box)
                          putWord8 0
                          putWord8 0
    get = do ampStrings <- doUntil null (get >>= (return .unAmpByteString))
             return ((AmpBox . flatToMap) (init ampStrings))


boxFromFile filepath =
    do content <- B.readFile filepath
       return ((unbox . decode) content)

