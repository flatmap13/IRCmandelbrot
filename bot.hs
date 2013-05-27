module Main where
import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Prelude hiding (catch)

server = "irc.freenode.org"
port = 6667
chan = "#tutbot-testing"
nick = "WOPRbot"

type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

main :: IO ()
main = bracket connect disconnect loop
	where
	disconnect = hClose . socket
	loop st = catch (runReaderT run st) (\(SomeException _) -> return ())

connect :: IO Bot
connect = notify $ do
	h <- connectTo server (PortNumber (fromIntegral port))
	hSetBuffering h NoBuffering
	return (Bot h)
	where
	notify a = bracket_
		(printf "Connecting to %s...\n" server >> hFlush stdout)
		(putStrLn "Done.")
		a

run :: Net ()
run = do
	write "NICK" nick
	write "USER" (nick ++ " 0 * :tutorial bot")
	write "JOIN" chan
	asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
	s <- init `fmap` io (hGetLine h)
	io (putStrLn s)
	if ping s then pong s else eval (clean s)
	where
	clean     = drop 1 . dropWhile (/= ':') . drop 1
	ping x    = "PING :" `isPrefixOf` x
	pong x    = write "PONG" (':' : drop 6 x) 

eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> io exitSuccess
eval x | "!id" `isPrefixOf` x = privmsg (drop 4 x)
eval _ = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
	h <- asks socket
	io $ hPrintf h "%s %s\r\n" s t
	io $ printf    "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO
