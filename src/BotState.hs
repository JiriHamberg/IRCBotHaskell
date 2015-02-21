module BotState where 

import Network
import GHC.Conc
import System.IO
import Control.Monad
import Control.Concurrent.Chan
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Text.Printf

import MsgParser
import IrcIO
import TicTacToe.Core

data BotData = BotData {
	botIn :: Chan ServerMessage,
	botOut :: Chan ClientMessage,	
	messageCount :: Int
}

type BotState = StateT BotData IO

newBotData :: Chan ClientMessage -> Handle -> IO BotData
newBotData serverInChan h = do
	c <- newChan
	forkIO $ forever $ do
		line <- hGetLine h 
		writeChan c (parseServerMsg line)

	return BotData {
		botIn = c,
		botOut = serverInChan,
		messageCount = 0
	}


handleServerMessage :: ServerMessage -> BotState ()
handleServerMessage (PING s) = do
	botData <- get
	put $ botData { messageCount = messageCount botData + 1 }
	liftIO $ writeChan (botOut botData) (PONG s)
	liftIO $ printf "got PING from server\n"
handleServerMessage (S_UNKNOWN s) = do
	botData <- get
	liftIO $ printf "got msg from server: %s\n" s
handleServerMessage (S_PRIVMSG sender _ target msg) = do
	botData <- get
	liftIO $ printf "got PRIVMSG from %s: %s\n" sender msg
	let answer = "I hear you"
	liftIO $ writeChan (botOut botData) (C_PRIVMSG { c_msgTarget = sender, c_msgBody = answer })

--Wait for next server message and act accordingly
botAct :: BotState ()
botAct = forever $ do
	botData <- get
	serverMsg <- liftIO $ readChan $ botIn botData
	handleServerMessage serverMsg