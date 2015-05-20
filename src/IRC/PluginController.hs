module IRC.PluginController ( runController ) where

import Control.Concurrent.Chan
import GHC.Conc
import Control.Monad

import IRC.Data
import IRC.IO 

import Plugins.Pong
import Plugins.Init
import Plugins.Logger


plugins :: [IO Plugin]
plugins = [ initPlugin,
	pongPlugin,
	loggerPlugin ] 


registerPlugin :: IRCConnection -> IO Plugin -> IO Plugin
registerPlugin (inC, outC) p = do
	plugin <- p
	newIn <- newChan
	newOut <- dupChan outC
	forkIO $ (action plugin) (newIn, newOut)
	return $ plugin { inC = newIn, outC = newOut }


runController :: IRCConnection -> IO ()
runController connection@(inIRC, outIRC) = do
	registered <- mapM (registerPlugin connection) plugins
	forever $ do
		msg <- readChan inIRC
		sequence $ map (\plugin -> writeChan (inC plugin) msg) $ filter (flip msgFilter $ msg) registered
