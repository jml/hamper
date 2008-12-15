import Amp
import Data.Binary
import qualified Data.ByteString.Lazy as B



add a b = makeBoxCommand (textToBytes "Sum") True
          (box [("a", (show a)), ("b", (show b))])


boxFromFile filepath =
    do content <- B.readFile filepath
       return ((unbox . decode) content)


dumpBox = show . B.unpack . encode


main = putStrLn (dumpBox (add 23 90))
