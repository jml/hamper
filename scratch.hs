import Control.Concurrent
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString.Lazy as B

import Random (getStdRandom, randomR)

rollDice :: IO Int
rollDice = getStdRandom $ randomR  $ (1, 6)

_FOO = 2


readline feeder = do c <- feeder
                     if c == '\n'
                        then return []
                        else do cs <- (readline feeder)
                                return (c : cs)


data Foo = MkFoo String
newtype Bar = MkBar String


data X = X { name :: String }



communicate = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn ("received " ++ show v)
  putStrLn "sending"
  putMVar m "wake up!"
