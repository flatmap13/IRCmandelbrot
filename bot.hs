module Main where
import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import System.Time
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
data Bot = Bot { socket :: Handle, startTime :: ClockTime }

main :: IO ()
main = bracket connect disconnect loop
    where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (\(SomeException _) -> return ())

connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
    where
    notify = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")

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
    if ping s then pong s else maybeEval (clean s)
    where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x) 
    clean  = drop 1 . dropWhile (/= ':') . drop 1

maybeEval :: String -> Net ()
maybeEval s | (nick ++ ": ") `isPrefixOf` s = eval (clean s)
            | otherwise                     = return ()
            where clean = dropWhile (== ' ') . drop 1 . dropWhile (/= ':') 

eval :: String -> Net ()
eval s | s == "uptime"       = uptime >>= privmsg
       | s == "quit"         = write "QUIT" ":EXITING" >> io exitSuccess
       | "id" `isPrefixOf` s = privmsg (dropWhile (== ' ') s)
       | otherwise           = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

uptime :: Net String
uptime = do
    now <- io getClockTime
    start <- asks startTime
    return . timeDiffToString $ diffClockTimes now start

io :: IO a -> Net a
io = liftIO

