module MsgParser where

import Text.Regex.Posix

data ServerMessage = 
	PING String | 
	S_PRIVMSG {
		nick :: String, 
		user :: String, 
		s_msgTarget :: String, --channel or user
		s_msgBody :: String 
	} | 
	S_UNKNOWN String

parseServerMsg :: String -> ServerMessage

parseServerMsg s 
	| s =~ "PING :(.*)" = parsePing s
	| s =~ ":(.*)!(.*) PRIVMSG (.*) :(.*)" = parsePrivMsg s
	| otherwise = S_UNKNOWN s

parsePing :: String -> ServerMessage
parsePing s = 
	PING (capt !! 1) 
		where capt = (s =~ "PING :(.*)" :: [[String]]) !! 0

parsePrivMsg :: String -> ServerMessage
parsePrivMsg s = 
	S_PRIVMSG {
			nick = capt !! 1,
			user = capt !! 2,
			s_msgTarget = capt !! 3,
			s_msgBody = capt !! 4
		} where capt = (s =~ ":(.*)!(.*) PRIVMSG (.*) :(.*)" :: [[String]]) !! 0 
