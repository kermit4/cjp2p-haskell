{-# LANGUAGE DeriveGeneric #-}
-- vim: set expandtab shiftwidth=2
import qualified Data.Set as Set
import Network.Socket (setSocketOption, SocketOption(Broadcast))
import Text.Read (readMaybe)

import qualified Data.Text.IO as TIO
import qualified Data.Aeson.KeyMap as KM

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

b = [SockAddrInet (fromIntegral port) (inet_addr ip) | (ip, port) <- [("148.71.89.128", 24254),("159.69.54.127", 24254)]]


main = do
  s <- socket AF_INET Datagram defaultProtocol
  setSocketOption s Broadcast 1
  bind s (SockAddrInet 24255 0)
  let peers = Set.fromList b
  loop s peers

loop :: Socket -> Set.Set SockAddr -> IO ()
loop s peers = do
  msg <- timeout 1000000 (recvFrom s 1024)
  mapM_ (\(k, v) -> TIO.putStrLn (T.pack (show (k, v)))) (Set.toList peers)
  case msg of
    Just (m, _) -> case decode (fromStrict m) :: Maybe Value of
      Just messages -> do
        let newPeers = Set.fromList [addr | p <- getPeers messages, let addr = parsePeer p, isRight addr, let Right a = addr]
       -- let peers' = Set.union peers newPeerMap
        loop s (Set.union peers newPeers)
      Nothing -> print "decode error" >> loop s peers
    Nothing -> do
      idx <- randomRIO (0, Set.size peers - 1)
      let (peerAddr, peerPort) = Set.elemAt idx peers
      TIO.putStrLn (T.pack "peers " <> T.pack (show peers))
      sendPeerRequest s peerAddr peerPort
      loop s peers
  where
    getPeers :: Value -> [SockAddr]
    getPeers (Array arr) = concatMap extractPeers (V.toList arr)
      where
        extractPeers (Object obj) = case KM.lookup (Key.fromString "Peers") obj of
          Just (Object peersObj) -> case KM.lookup (Key.fromString "peers") peersObj of
            Just (Array peersArr) -> catMaybes [parseSockAddr p | String p <- V.toList peersArr]
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

parseSockAddr :: T.Text -> Maybe SockAddr
parseSockAddr t = case T.splitOn ":" t of
  [ipT, portT] -> do
    port <- readMaybe (T.unpack portT)
    addr:_ <- getAddrInfo (Just (defaultHints {addrFlags = [AI_NUMERICHOST]}) ) (Just (T.unpack ipT)) (Just (show port))
    return (addrAddress addr)
  _ -> Nothing

