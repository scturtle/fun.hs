{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Binary(encode, decode)
import Data.Binary.Get (runGet, getWord16be, getWord32le)
import Data.Binary.Put (runPut, putWord16be, putWord32le)
import Data.IP (fromHostAddress, fromHostAddress6)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8

import Conduit
import Data.Conduit.Network
import Control.Concurrent.Async (race_)
import Network.Socket (SockAddr(..))


initialize :: ConduitM ByteString ByteString IO (ByteString, Int)
initialize = do
  Just _init <- await
  yield "\x05\x00" -- no auth
  Just (proto, more) <- (B.splitAt 4 <$>) <$> await
  let typ = B.index proto 3
  let (host, rest) =
        case typ of
          -- domain
          3 -> let len = fromIntegral $ B.head more
               in  B.splitAt len (B.tail more)
          -- IPv4
          1 -> let (ip, rst) = B.splitAt 4 more
                   addr = fromHostAddress . runGet getWord32le . L.fromStrict $ ip
               in  (B8.pack . show $ addr, rst)
          -- IPv6
          4 -> let (ip, rst) = B.splitAt 16 more
                   addr = fromHostAddress6 . decode . L.fromStrict $ ip
               in  (B8.pack . show $ addr, rst)
  let port = fromIntegral . runGet getWord16be . L.fromStrict $ B.take 2 rest
  return (host, port)


main :: IO ()
main =
  runTCPServer (serverSettings 8000 "*") $ \client -> do
    (clientSource, (remoteHost, remotePort)) <-
      appSource client $$+ initialize `fuseUpstream` appSink client
    runTCPClient (clientSettings remotePort remoteHost) $ \remote -> do
      let (typ, hostBs, port) =
            case appSockAddr remote of
              SockAddrInet portNum host ->
                ("\x01", runPut (putWord32le host), portNum)
              SockAddrInet6 portNum _ host _ ->
                ("\x04", encode host, portNum)
      let msg =  "\x05\x00\x00" <> typ <> hostBs
              <> runPut (putWord16be . fromInteger $ toInteger port)
      -- answer the remote host & port
      clientSource $$++ yield (L.toStrict msg) =$ appSink client
      -- tunnel between client and remote
      race_ (clientSource $$+- appSink remote)
            (appSource remote $$ appSink client)
