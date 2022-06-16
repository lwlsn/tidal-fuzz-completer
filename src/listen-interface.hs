import Sound.OSC.FD as O
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Network.Socket as N
import qualified Sound.Tidal.Tempo as Tempo
{-
https://github.com/tidalcycles/tidal-listener/wiki
-}


import Sound.Tidal.Ngrams
import Sound.Tidal.Types
import Sound.Tidal.Tokeniser

import System.Environment

data State = State {sLocal :: UDP,
                    sRemote :: N.SockAddr
                   }

listenPort = 12000
remotePort = 8888

main :: IO ()
main = listen

listen :: IO ()
listen = do -- listen
            (remote_addr:_) <- N.getAddrInfo Nothing (Just "127.0.0.1") Nothing
            local <- udpServer "127.0.0.1" listenPort
            putStrLn $ "Listening for OSC commands on port " ++ show listenPort
            putStrLn $ "☆♬○♩●♪✧♩((ヽ( ᐛ )ﾉ))♩✧♪●♩○♬☆ Sending replies on port " ++ show remotePort
            let (N.SockAddrInet _ a) = N.addrAddress remote_addr
                remote  = N.SockAddrInet (fromIntegral remotePort) a
                st      = State local remote
            loop st
              where
                loop st =
                  do -- wait for, read and act on OSC message
                     m <- recvMessage (sLocal st)
                     st' <- act st m
                     loop st'

act :: State -> Maybe O.Message -> IO State

act st (Just (Message "/slider2DValX" [O.Float val])) = do
  r <- openUDP  "127.0.0.1" remotePort
  putStrLn $ "Received x-coordinate"
--   codeOut <- returnFunc
--   let replace = map (\c -> if c=='\"' then '\''; else c)
--   sendMessage r $ Message "/reply" [string ("[\"" ++ replace (sCode codeOut) ++ "\"]")]
  --do O.sendTo (sLocal st) (O.p_message "/pong" []) (sRemote st)
  return st

act st (Just (Message "/slider2DValY" [O.Float val])) = do
  r <- openUDP  "127.0.0.1" remotePort
  putStrLn $ "Received y-coordinate"
  return st

act st (Just (Message "/sliderVal" [O.Int32 val])) = do
  r <- openUDP  "127.0.0.1" remotePort
  putStrLn $ "Received temperature"
  return st

act st Nothing = do putStrLn "not a message?"
                    return st
act st (Just m) = do putStrLn $ "Unhandled message: " ++ show m
                     return st


sCode :: Code -> String
sCode = show


replaceChar i o = map (\c -> if c==i then o; else c)
