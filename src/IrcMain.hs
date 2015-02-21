module IrcMain where

import Control.Concurrent.Chan
import Control.Monad.State

import IrcConfig
import IrcIO

import MsgParser
import BotState


main :: IO()
main = do
	serverHandle <- connect
	clientOut <- newChan
	runServerDispatcher clientOut serverHandle
	botData <- newBotData clientOut serverHandle
	runStateT botAct botData
	return ()