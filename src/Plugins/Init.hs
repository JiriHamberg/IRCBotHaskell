module Plugins.Init ( initPlugin ) where

import Control.Concurrent.Chan
import GHC.Conc

import IRC.IO
import IRC.Data
import qualified IRC.Config as C


initPlugin :: IO Plugin
initPlugin = do
	plugin <- pluginDefault
	return $ plugin {
		action = initAction,
		msgFilter = initFilter
	}


initFilter :: Msg -> Bool
initFilter msg = False


initAction :: IRCConnection -> IO()
initAction (inC, outC) = do
	writeChan outC $ msgDefault { command = "NICK", params = [C.nick] }
	writeChan outC $ msgDefault { command = "USER", params = [C.nick, "0", "*", ":tutorial", "bot"] }
	writeChan outC $ msgDefault { command = "JOIN", params = [C.chan] }
	print $ "Connected to channel"