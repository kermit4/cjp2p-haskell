-- vim: set expandtab shiftwidth=2 tabstop=2:
{-# LANGUAGE DeriveGeneric #-}
import Network.Socket (setSocketOption, SocketOption(Broadcast))
import Text.Read (readMaybe)
import qualified Data.Text.IO as TIO
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Monad (forM)
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


data PeerRequest = PleaseSendPeers deriving (Show)
instance ToJSON PeerRequest where
  toJSON PleaseSendPeers = toJSON [object [(fromString "PleaseSendPeers" .= object [])]]

b = [ ("148.71.89.128", 24254),("159.69.54.127", 24254)]

main = do
  s <- socket AF_INET Datagram defaultProtocol
  setSocketOption s Broadcast 1
  bind s (SockAddrInet 24255 0)
  let peers = Map.fromList b
  loop s peers

-- Define a function that takes a tuple (k, v) as input
printPeer :: (String, Int) -> IO ()
printPeer tuple = do
  -- Extract the key (k) and value (v) from the tuple
  let k = fst tuple
  let v = snd tuple
  
  -- Convert the tuple to a string
  let tupleString = show (k, v)
  
  -- Convert the string to a Text value
  let textValue = T.pack tupleString
  
  -- Print the Text value to the console
  TIO.putStrLn textValue



loop s peers = do
  msg <- timeout 1000000 (recvFrom s 1024)
  -- Apply the printPeer function to each element in the list
  mapM_ printPeer (Map.toList peers)

  case msg of
    Just (m, _) -> case decode (fromStrict m) of
    -- Try to decode the message as JSON
      -- If decoding succeeds, process the message
      Just messages -> do
        -- Extract peer addresses from the message
        let peerAddresses = getPeers messages
        
        -- Create a list of (IP, port) tuples
        let newPeers = []
        newPeers' <- forM peerAddresses $ \peerAddress -> do
          -- Split the address into IP and port
          let parts = T.splitOn (T.pack ":") peerAddress
          let ip = parts !! 0
          let portS = parts !! 1
          
          -- Parse the port number
          case readMaybe (T.unpack portS) of
            Just port -> do
              -- Return the peer
              return [(T.unpack ip, port)]
            Nothing -> do
              -- Ignore invalid port numbers
              return []
        
        -- Flatten the list of lists
        let newPeers = concat newPeers'
        
        -- Create a map from the new peers
        let newPeerMap = Map.fromList newPeers
        
        -- Merge the new peers with the existing peers
        let updatedPeers = Map.union peers newPeerMap
        
        -- Continue processing with the updated peers
        loop s updatedPeers
      
      -- If decoding fails, print an error and continue
      Nothing -> do
        print "decode error"
        loop s peers


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
            Just (Array peersArr) ->
              let peers = V.toList peersArr
              let strings = []
              for each peer in peers:
                if peer is a String p:
                  add p to strings
              return strings

  
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

