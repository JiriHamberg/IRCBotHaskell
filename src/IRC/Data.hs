module IRC.Data ( IRCConnection, ToIRC, dump, Msg (..), parseMsg, msgDefault, Plugin(..), pluginDefault ) where

import Control.Concurrent.Chan
import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.List
import Text.Printf


type IRCConnection = (Chan Msg, Chan Msg)


class ToIRC a where
	dump :: a -> String



data Msg = Msg {
	prefix :: Maybe String,
	command :: String,
	params :: [String]
} deriving (Eq, Show)

msgDefault = Msg { prefix = Nothing, command = "", params = [] }


instance ToIRC Msg where
	dump (Msg pref cmd params) = printf "%s %s %s" fpref cmd fparams
		where
			fpref = maybe "" (\s -> ":" ++ s) pref
			fparams = intercalate " " params



data Plugin = Plugin {
	inC :: Chan Msg,
	outC :: Chan Msg,
	action :: IRCConnection -> IO (),
	msgFilter :: Msg -> Bool
}

pluginDefault :: IO Plugin
pluginDefault = do
	newIn <- newChan
	newOut <- newChan
	return $ Plugin {
		inC = newIn,
		outC = newOut,
		action = \c -> return (),
		msgFilter = \m -> False
	}


parseMsg :: String -> Either ParseError Msg 
parseMsg input = parse msgParser (error "Could not parse Msg") input

msgParser = do
	prefix <- prefixParser
	command <- commandField 
	params <- optionMaybe whitespace >> paramsParser
	optionMaybe whitespace
	return $ Msg prefix command params

whitespace = do
	first <- string " "
	rest <- many (char ' ')
	return $ first ++ rest

commandField = do
	first <- noneOf " :\n\r"
	rest <- many $ noneOf " :\n\r"
	return $ first : rest

textField = do
	first <- noneOf " \n\r"
	rest <- many $ noneOf " \n\r"
	return $ first : rest

prefixParser = do
	prefix <- optionMaybe $ char ':' >> textField
	if isJust prefix then 
		whitespace >> return prefix
	else
		return Nothing

paramsParser = sepBy textField whitespace


main = do
	let msg = parseMsg ":foobar PING"
	print msg
	print $ case msg of
		Right msg -> dump msg 
		_ -> "parse error"