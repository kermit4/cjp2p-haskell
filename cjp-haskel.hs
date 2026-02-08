{-# LANGUAGE DeriveGeneric #-}
import Control.Monad (forever)
import Data.ByteString (fromStrict)
import Network.Socket
import Network.Socket.ByteString
import Data.ByteString.Lazy (toStrict)
import Data.Aeson
import GHC.Generics

data P = P { a :: String, p :: Int } deriving (Generic, Show)
instance ToJSON P
instance FromJSON P

b = [("148.71.89.128", 24254), ("159.69.54.127", 24254)]

main = do
  s <- socket AF_INET Datagram defaultProtocol
  bind s (SockAddrInet 24255 0)
  mapM_ (f s) b
  f' s

f s (a, p) = do
  let m = P a p
  sendTo s (toStrict $ encode m) =<< a'
  where
    a' = do
      h <- getAddrInfo (Just (defaultHints { addrSocketType = Datagram })) (Just a) (Just $ show p)
      return $ addrAddress $ head h

f' s = forever $ do
  (m, _) <- recvFrom s 1024
  case eitherDecode (fromStrict m) of
    Right (P a p) -> print ("D", a, p)
    Left e -> print e

