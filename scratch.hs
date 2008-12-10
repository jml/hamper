
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString.Lazy as B

import Random (getStdRandom, randomR)

rollDice :: IO Int
rollDice = getStdRandom $ randomR  $ (1, 6)

_FOO = 2
