-- AMP implementation.
-- See twisted.protocols.amp.

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import qualified Codec.Binary.UTF8.String as UTF8


ask = "_ask"
answer = "_answer"
command = "_command"
error = "_error"
error_code = "_error_code"
error_description = "_error_description"
unknown_error_code = "UNKNOWN"
unhandled_error_code = "UNHANDLED"

max_key_length = 0xff
max_value_length = 0xffff


encodeToBytes string = B.pack (UTF8.encode string)
decodeBytes string = UTF8.decode (B.unpack string)

class Argument_ a where
    toByteString   :: a -> B.ByteString
    fromByteString :: B.ByteString -> a

instance Argument_ Integer where
    toByteString   = \x -> encodeToBytes (show x)
    fromByteString = \x -> read (decodeBytes x) :: Integer

instance Argument_ String where
    toByteString   = encodeToBytes
    fromByteString = decodeBytes


type AmpBox = Map.Map B.ByteString B.ByteString


data Argument = forall a. Argument_ a => Argument a
type ArgumentList = Map.Map String Argument


boxTerminator = Binary.encode (0 :: Integer)

serializeBytes string = B.append (Binary.encode (B.length string)) string

serializeEntry (first, second) = B.append (serializeBytes first) (serializeBytes second)

serializeBox :: AmpBox -> B.ByteString
serializeBox box = B.append (B.concat $ map serializeEntry (Map.toList box)) boxTerminator


toBoxTuple :: (Argument_ a) => String -> a -> (B.ByteString, B.ByteString)
toBoxTuple name value = ((encodeToBytes name), (toByteString value))

toBox argumentList = Map.fromList [toBoxTuple name value | (name, value) <- Map.toList argumentList]


-- Need mapping from String (parameter name) to ParameterType
-- Need mapping from String to Parameter*
-- Take both of these, and convert to a box
-- Take the box and serialize it (maybe with additional AMP pragma)
