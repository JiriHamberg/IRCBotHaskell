module IRC.IO ( connectIRC, IRCConnection ) where

import Network
import System.IO
import Text.Printf
import Control.Monad
import GHC.Conc
import Control.Concurrent.Chan
import Text.ParserCombinators.Parsec
import Data.Either

import qualified IRC.Config as C
import IRC.Data


sendIRC :: ToIRC a => Handle -> a -> IO ()
sendIRC h toSend = do
	hPutStrLn h $ dump toSend 


makeMsgChannels :: Handle -> IO IRCConnection
makeMsgChannels h = do
	inChan <- newChan
	outChan <- newChan
	
	-- Consumer thread for message output channel 
	forkIO $ getChanContents outChan >>= mapM_ (sendIRC h)
	
	-- Producer thread for message input channel
	forkIO $ do
		contents <- hGetContents h
		let msgs = rights $ map parseMsg $ lines contents  
		mapM_ (writeChan inChan) msgs
	
	return $ (inChan, outChan)


connectIRC :: IO IRCConnection
connectIRC = do
	h <- connectTo C.server (PortNumber (fromIntegral C.port))
	hSetBuffering h NoBuffering
	makeMsgChannels h
