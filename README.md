This was mostly AI made, and it has a difficult to fix bug where it only remembers the LAST port for a host, because its a Map not a Set, but thats not tooo bad.

All this does is receive peers and ask for more peers.  I don't know Haskell.  I was just taking a look at it really.

Attempts have been made to convert it to a Set of SockAddr in the branch store_peers_as_sockaddr
