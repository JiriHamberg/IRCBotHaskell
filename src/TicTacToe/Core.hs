module TicTacToe.Core where

import Data.Array
import Data.List
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

data Symbol = Empty | X | O deriving (Eq, Show)

showSymbol :: Symbol -> String
showSymbol Empty = " "
showSymbol X = "x"
showSymbol O = "o"

data GameData = GameData {
	--gameIn :: Chan (Int, Int),
	--gameOut :: Chan String,
	grid :: Array (Int, Int) Symbol, 
	turn :: Symbol
} deriving (Show)


--showGameState :: GameData -> String
--showGameState (GameData _ _ g t) = undefined



newGame :: GameData
newGame =
	--cIn <- newChan
--	cOut <- newChan
	GameData {
		grid = array ((1,1),(3,3)) [((x,y), Empty) | x <- [1..3], y <- [1..3]],
		turn = X
	}

type GameState = StateT GameData IO

nextTurn :: Symbol -> Symbol
nextTurn X = O
nextTurn O = X

validMove :: (Int, Int) -> GameState Bool
validMove index = get >>= \x -> return ((grid x) ! index == Empty)

gameOver :: GameState Bool
gameOver = do
	d <- get
	return $ all ((/=) Empty) (elems $ grid d) 

makeMove :: (Int, Int) -> GameState ()
makeMove index = do
	ok <- validMove index
	if ok then do
		gameData <- get
		put GameData {
			grid = (grid gameData) // [(index, turn gameData)],
			turn = nextTurn (turn gameData)
		}
	else return ()

--runGame :: GameState ()
--runGame = do
--	move <- liftIO $ readChan (gameIn get)
--	makeMove move
--	isGameOver <- gameOver
--	if isGameOver then () else runGame

