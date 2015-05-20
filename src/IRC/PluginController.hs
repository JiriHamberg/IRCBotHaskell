module IRC.PluginController ( runController ) where

import Control.Concurrent.Chan
import GHC.Conc
import Control.Monad


import IRC.Data
import IRC.IO 


import Plugins.Pong
import Plugins.Init
import Plugins.Logger



-- (IRCConnection -> IO(), Msg -> Bool)


plugins :: [IO Plugin]
plugins = [ initPlugin,
	pongPlugin,
	loggerPlugin ] 


registerPlugin :: IRCConnection -> IO Plugin -> IO Plugin
registerPlugin (inC, outC) p = do
	plugin <- p
	newIn <- newChan
	-- newIn <- dupChan inC
	newOut <- dupChan outC
	-- newOut <- newChan
	forkIO $ (action plugin) (newIn, newOut)
	return $ plugin { inC = newIn, outC = newOut }


runController :: IRCConnection -> IO ()
runController connection@(inIRC, outIRC) = do
	registered <- mapM (registerPlugin connection) plugins
	forever $ do
		msg <- readChan inIRC
		sequence $ map (\plugin -> writeChan (inC plugin) msg) $ filter (flip msgFilter $ msg) registered
