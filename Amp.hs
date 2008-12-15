module Amp (AmpBox, box, makeBoxCommand, textToBytes, unbox) where

import Control.Monad
import Data.Binary
import qualified Data.Map as Map
import qualified Codec.Binary.UTF8.String as UTF8
import Util


-- Convert between text and UTF8-encoded bytes.
textToBytes = UTF8.encode
u = textToBytes
bytesToText = UTF8.decode


-- AMP Constants
_ASK = u "_ask"
_ANSWER = u "_answer"
_COMMAND = u "_command"
_ERROR = u "_error"
_ERROR_CODE = u "_error_code"
_ERROR_DESCRIPTION = u "_error_description"
_UNKNOWN_ERROR_CODE = u "UNKNOWN"
_UNHANDLED_ERROR_CODE = u "UNHANDLED"


-- The basic unit of AMP is a length-prefix byte string.
data AmpByteString = AmpByteString [Word8]
_unAmpByteString (AmpByteString string) = string

instance Binary AmpByteString where
    put (AmpByteString bytes) = do put (fromIntegral (length bytes) :: Word16)
                                   mapM_ put bytes

    get = do numBytes <- get :: Get Word16
             do payload <- replicateM (fromIntegral numBytes) getWord8
                return (AmpByteString payload)


newtype AmpBox = AmpBox (Map.Map [Word8] [Word8])
_unAmpBox (AmpBox box) = box


instance Binary AmpBox where
    put (AmpBox box) = do mapM_ (put . AmpByteString) (mapToFlat box)
                          put (0 :: Word16)
    get = do ampStrings <- doUntil null (get >>= (return . _unAmpByteString))
             return ((AmpBox . flatToMap) (init ampStrings))


_addToBox key value box = AmpBox (Map.insert key value (_unAmpBox box))
makeBoxCommand command False box = _addToBox _COMMAND command box
makeBoxCommand command True box =
    _addToBox _ASK (u "unique!") (makeBoxCommand command False box)


-- Because it's a pain to play with AmpBoxes in the interpreter, these helpers
-- convert from [(String, String)] -> AmpBox and back.
box x = (AmpBox . Map.fromList) (map (double textToBytes) x)
unbox (AmpBox x) = map (double bytesToText) (Map.toList x)
