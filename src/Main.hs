module Main where

import Control.Concurrent.Chan
import Control.Monad.State
import GHC.Conc

import IRC.IO
import IRC.PluginController


main :: IO ()
main = do
	connection <- connectIRC
	runController connection
		