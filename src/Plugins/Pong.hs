module Plugins.Pong ( pongPlugin ) where

import Control.Monad
import Control.Concurrent.Chan
import GHC.Conc

import IRC.IO
import IRC.Data


pongPlugin :: IO Plugin
pongPlugin = do
	plug <- pluginDefault
	return $ plug { 
		action = pongAction,
		msgFilter = pongFilter
	}


pongFilter :: Msg -> Bool
pongFilter (Msg _ "PING" _ ) = True	
pongFilter _ = False


pongAction :: IRCConnection -> IO()
pongAction (inC, outC) = forever $ do
	msg <- readChan inC
	case msg of 
		Msg _ "PING" (p : ps)  -> do
			let responce = msgDefault { command = "PONG", params = [p] }
			print $ "PONG SENT"
			writeChan outC responce
		_ -> return ()


