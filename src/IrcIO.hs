module IrcIO where

import Network
import System.IO
import Text.Printf
import Control.Monad
import GHC.Conc
import Control.Concurrent.Chan

import IrcConfig

data ClientMessage = 
	PONG String |
	WHOIS String |
	JOIN String |
	C_PRIVMSG {
		c_msgTarget :: String,
		c_msgBody :: String
	} |
	C_UNKNOWN String


decodeClientMessage :: ClientMessage -> String
decodeClientMessage (PONG s) = printf "PONG :%s\r\n" s
decodeClientMessage (C_PRIVMSG target msg) = printf "PRIVMSG %s :%s\r\n" target msg
decodeClientMessage _ = undefined


connect :: IO Handle
connect = do
	h <- connectTo server (PortNumber (fromIntegral port))
	hSetBuffering h NoBuffering
	write h "NICK" nick
	write h "USER" (nick ++ " 0 * :tutorial bot")
	write h "JOIN" chan
	return h


write :: Handle -> String -> String -> IO ()
write h s t = do
	hPrintf h "%s %s\r\n" s t
	printf    "> %s %s\n" s t


runServerDispatcher :: Chan ClientMessage -> Handle -> IO ThreadId 
runServerDispatcher clientOut serverHandle = forkIO $ forever $ do
	clientMsg <- readChan clientOut
	hPrintf serverHandle (decodeClientMessage clientMsg)
