-- vim: set expandtab shiftwidth=2
{-# LANGUAGE DeriveGeneric #-}
import Network.Socket (setSocketOption, SocketOption(Broadcast))
import Text.Read (readMaybe)

import qualified Data.Text.IO as TIO
import qualified Data.Aeson.KeyMap as KeyMap

import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad (forever)
import Data.ByteString (fromStrict)
import Network.Socket
import Network.Socket.ByteString
import Data.ByteString.Lazy (toStrict)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Key ( fromString )
import GHC.Generics
import System.Timeout
import Control.Concurrent
import System.Random (randomRIO)
import qualified Data.Map as Map

data P = P { a :: String, p :: Int } deriving (Generic, Show)
instance ToJSON P
instance FromJSON P

data PeerRequest = PleaseSendPeers deriving (Show)
instance ToJSON PeerRequest where
  toJSON PleaseSendPeers = toJSON [object [(fromString "PleaseSendPeers" .= object [])]]

data PeerResponse = PeerResponse { peers :: [String] } deriving (Generic, Show)
instance FromJSON PeerResponse

b = [ ("148.71.89.128", 24254),("159.69.54.127", 24254)]

main = do
  s <- socket AF_INET Datagram defaultProtocol
  setSocketOption s Broadcast 1
  bind s (SockAddrInet 24255 0)
  let peers = Map.fromList b
  loop s peers

loop :: Socket -> Map.Map String Int -> IO ()
loop s peers = do
  msg <- timeout 1000000 (recvFrom s 1024)
  mapM_ (\(k, v) -> TIO.putStrLn (T.pack (show (k, v)))) (Map.toList peers)
  case msg of
    Just (m, _) -> case decode (fromStrict m) :: Maybe Value of
      Just messages -> do
        let newPeers = [(T.unpack ip, port) | p <- getPeers messages, let (ip : portS : _) = T.splitOn (T.pack ":") p, let Just port = readMaybe (T.unpack portS)]
        let newPeerMap = Map.fromList newPeers
        let peers' = Map.union peers newPeerMap
        loop s (Map.union peers newPeerMap)
      Nothing -> print "decode error" >> loop s peers
    Nothing -> do
      idx <- randomRIO (0, Map.size peers - 1)
      let (peerAddr, peerPort) = Map.elemAt idx peers
      TIO.putStrLn (T.pack "peers " <> T.pack (show peers))
      sendPeerRequest s peerAddr peerPort
      loop s peers
  where
    getPeers :: Value -> [T.Text]
    getPeers (Array arr) = concatMap extractPeers (V.toList arr)
      where
        extractPeers (Object obj) = case KeyMap.lookup (Key.fromString "Peers") obj of
          Just (Object peersObj) -> case KeyMap.lookup (Key.fromString "peers") peersObj of
            Just (Array peersArr) -> [p | String p <- V.toList peersArr]
            _ -> []
          _ -> []
        extractPeers _ = []
    getPeers _ = []
    parsePeer p = case break (==':') p of
      (ip, ':' : port) -> (ip, read port)
      _ -> error "Invalid peer format"


sendPeerRequest s peerAddr peerPort = do
  let request = encode PleaseSendPeers
  addr:_ <- getAddrInfo (Just (defaultHints { addrSocketType = Datagram })) (Just peerAddr) (Just (show peerPort))
  sendTo s (toStrict request) (addrAddress addr)

