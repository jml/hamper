import Amp
import Data.Binary
import Data.Map
import qualified Data.ByteString.Lazy as B


_addBox a b = (box [("a", show (a :: Integer)), ("b", show (b :: Integer))])

add h a b = do
    reply <- ampFunction h "Sum" (_addBox a b)
    return (read ((fromList (unbox reply)) ! "total") :: Integer)


boxFromFile filepath =
    do content <- B.readFile filepath
       return ((unbox . decode) content)


main = do
  handle <- connectTCP "localhost" "1234"
  reply <- add handle 15 98
  putStrLn (show reply)
  disconnect handle
