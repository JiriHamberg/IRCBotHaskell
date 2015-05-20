module Plugins.Logger ( loggerPlugin ) where


import Control.Monad
import Control.Concurrent.Chan
import GHC.Conc

import IRC.IO
import IRC.Data


loggerPlugin :: IO Plugin
loggerPlugin = do
	plugin <- pluginDefault
	return $ plugin {
		action = loggerAction,
		msgFilter = loggerFilter
	}


loggerFilter :: Msg -> Bool
loggerFilter msg = True


loggerAction :: IRCConnection -> IO()
loggerAction (inC, outC) = forever $ do
	msg <- readChan inC
	print $ "Logger: " ++ show msg