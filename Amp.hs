-- AMP implementation.
-- See twisted.protocols.amp.

import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import qualified Codec.Binary.UTF8.String as UTF8


mapMap keyFunc valueFunc map =
    Map.fromList [ (keyFunc key, valueFunc value) | (key, value) <- Map.toList map ]

double f (x, y) = (f(x), f(y))

-- Convert between text and UTF8-encoded bytes.
textToBytes = B.pack . UTF8.encode
bytesToText = UTF8.decode . B.unpack

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

newtype AmpBox = AmpBox (Map.Map B.ByteString B.ByteString)
unAmpBox (AmpBox box) = box

max_key_length = 0xff
max_value_length = 0xffff

serializeBoxBytes bytes = (Binary.encode . B.length) bytes `B.append` bytes
serializeBoxKey = serializeBoxBytes
serializeBoxValue = serializeBoxBytes
serializeBoxPair (key, value) =
    (serializeBoxKey key) `B.append` (serializeBoxValue value)
boxTerminator = Binary.encode (0 :: Integer)

serializeBox :: AmpBox -> B.ByteString
serializeBox (AmpBox box) =
    B.concat (map serializeBoxPair (Map.toList box)) `B.append` boxTerminator


-- Because it's a pain to play with AmpBoxes in the interpreter, these helpers
-- convert from [(String, String)] -> AmpBox and back.
box x = (AmpBox . Map.fromList) (map (double textToBytes) x)
unbox (AmpBox x) = map (double bytesToText) (Map.toList x)


-- Commands

makeBoxCommand command box = AmpBox (Map.insert _COMMAND command (unAmpBox box))

-- Need mapping from String (parameter name) to ParameterType
-- Need mapping from String to Parameter*
-- Take both of these, and convert to a box
-- Take the box and serialize it (maybe with additional AMP pragma)
