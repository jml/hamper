-- AMP implementation.
-- See twisted.protocols.amp.

import Data.Bits
import Data.Word
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import qualified Codec.Binary.UTF8.String as UTF8


mapMap keyFunc valueFunc map =
    Map.fromList [ (keyFunc key, valueFunc value) | (key, value) <- Map.toList map ]

double f (x, y) = (f(x), f(y))

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

packLength n = (B.unpack . Binary.encode) (fromIntegral n :: Word16)

serializeBoxBytes bytes = (packLength (length bytes)) ++ bytes
serializeBoxKey = serializeBoxBytes
serializeBoxValue = serializeBoxBytes
serializeBoxPair (key, value) =
    (serializeBoxKey key) ++ (serializeBoxValue value)

boxTerminator = serializeBoxKey ""

serializeBox :: AmpBox -> [Word8]
serializeBox (AmpBox box) =
    concat (map serializeBoxPair (Map.toList box)) ++ boxTerminator


-- Because it's a pain to play with AmpBoxes in the interpreter, these helpers
-- convert from [(String, String)] -> AmpBox and back.
box x = (AmpBox . Map.fromList) (map (double textToBytes) x)
unbox (AmpBox x) = map (double bytesToText) (Map.toList x)


-- Commands

makeBoxCommand command box = AmpBox (Map.insert _COMMAND command (unAmpBox box))

add a b = makeBoxCommand (textToBytes "Sum") (box [("a", (show a)), ("b", (show b))])

-- Need mapping from String (parameter name) to ParameterType
-- Need mapping from String to Parameter*
-- Take both of these, and convert to a box
-- Take the box and serialize it (maybe with additional AMP pragma)
