{-# LANGUAGE DeriveGeneric #-}
import Control.Monad (forever)
import Data.ByteString (fromStrict)
import Network.Socket
import Network.Socket.ByteString
import Data.ByteString.Lazy (toStrict)
import Data.Aeson
import Data.Aeson.Key (fromString)
import GHC.Generics
import System.Timeout
import Control.Concurrent
import System.Random (randomRIO)

data P = P { a :: String, p :: Int } deriving (Generic, Show)
instance ToJSON P
instance FromJSON P

data PeerRequest = PleaseSendPeers deriving (Show)
instance ToJSON PeerRequest where
  toJSON PleaseSendPeers = toJSON [object [(fromString "PleaseSendPeers" .= object [])]]

b = [("148.71.89.128", 24254), ("159.69.54.127", 24254)]

main = do
  s <- socket AF_INET Datagram defaultProtocol
  bind s (SockAddrInet 24255 0)
  forkIO $ forever $ do
    threadDelay 1000000 -- 1 second
    idx <- randomRIO (0, length b - 1)
    let (peerAddr, peerPort) = b !! idx
    sendPeerRequest s peerAddr peerPort
  forever $ do
    (m, _) <- recvFrom s 1024
    case eitherDecode (fromStrict m) of
      Right (P a p) -> print ("D", a, p)
      Left e -> print e

sendPeerRequest s peerAddr peerPort = do
  let request = encode PleaseSendPeers
  addr <- getAddrInfo (Just (defaultHints { addrSocketType = Datagram })) (Just peerAddr) (Just $ show peerPort)
  sendTo s (toStrict request) (addrAddress $ head addr)

