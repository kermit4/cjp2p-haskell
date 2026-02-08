import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.Key (fromString)
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)
import Data.Word (Word8)
import System.Timeout (timeout)
import Network.IP.Addr (IP46(IPv4),IP)

data PeerRequest = PleaseSendPeers
  deriving (Show)

instance ToJSON PeerRequest where
  toJSON PleaseSendPeers = object [fromString "PleaseSendPeers" .= object []]

main :: IO ()
main = do
  -- Create a UDP socket
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock (SockAddrInet 0 0)

  let peers = Map.fromList [("148.71.89.128", 24254), ("159.69.54.127", 24254)]
  loop sock peers

loop :: Socket -> Map String Int -> IO ()
loop sock peers = do
  -- Receive a message with timeout
  msg <- timeout 1000000 (recvFrom sock 1024)
  case msg of
    Just (msg, sockaddr) -> do
      print ("Received from: " ++ show sockaddr)
      print msg
    Nothing -> print "Timeout"

  print "Sending request..."
  -- Send request for more peers to a random peer
  idx <- randomRIO (0, Map.size peers - 1)
  let (peerAddr, peerPort) = Map.elemAt idx peers
  print ("Peer addr: " ++ peerAddr)
  print ("Peer port: " ++ show peerPort)

  case fromString peerAddr :: Maybe (IP 'IPv4) of
    Just addr -> do
      let peerSockAddr = SockAddrInet (fromIntegral peerPort) (fromIntegral (fromIPv4 addr))
          request = encode $ PleaseSendPeers
      print ("Sending to: " ++ show peerSockAddr)
      sendTo sock (BSL.toStrict request) peerSockAddr
    Nothing -> print "Invalid address"

  loop sock peers

