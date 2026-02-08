import Network.Socket
import Network.Socket.ByteString (recvFrom)
import qualified Data.ByteString as BS

main :: IO ()
main = do
  -- Create a UDP socket
  sock <- socket AF_INET Datagram defaultProtocol
  
  -- Bind to a local address (0.0.0.0:0 lets OS choose a port)
  bind sock (SockAddrInet 0 0)
  
  -- Receive a message
  (msg, sockaddr) <- recvFrom sock 1024
  
  -- Print the message and sender's IP/port
  case sockaddr of
    SockAddrInet port host -> do
      print ("Received from: " ++ show (host, port))
      print msg
    _ -> print "Unknown address"

