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
import Control.Concurrent
import Prelude hiding (catch)
import Mandelbrot

server = "irc.freenode.org"
port = 6667
chan = "#tutbot-testing"
nick = "WOPRbot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, startTime :: ClockTime }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
    where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (\(SomeException _) -> return ())

-- Connect to the server and return the initial bot state
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

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else maybeEval (clean s)
    where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x) 
    clean  = drop 1 . dropWhile (/= ':') . drop 1

-- Check whether the received message is addressed at the bot
-- If so, evaluate the message
maybeEval :: String -> Net ()
maybeEval s | (nick ++ ": ") `isPrefixOf` s = eval (clean s)
            | otherwise                     = return ()
            where clean = dropWhile (== ' ') . drop 1 . dropWhile (/= ':') 

-- Dispatch a command
eval :: String -> Net ()
eval s | s == "uptime"               = uptime >>= privmsg
       | s == "quit"                 = write "QUIT" ":EXITING" >> io exitSuccess
       | "id" `isPrefixOf` s         = privmsg ((dropWhile (== ' ') . drop 2) s)
       | "mandelbrot" `isPrefixOf` s = evalBrot s
       | otherwise                   = return ()

-- Evaluates whether the 'mandelbrot' command is valid
-- If so, the command is dispatched
evalBrot :: String -> Net ()
evalBrot s
    | length args /= 2 = privmsg "Usage: mandelbrot width height" 
    | otherwise = showBrot size $ createBrot size
    where 
    args = drop 1 $ words s
    size = Size (read $ head args) (read $ args !! 1)

-- Show an (ascii) mandelbrot
-- Sends the lines with a delay of 1 sec (to prevent flooding)
showBrot :: Size -> String -> Net ()
showBrot _ [] = return ()
showBrot scr @ (Size w _) str = do
    when (any (' ' /=) line) $ privmsg line
    io $ threadDelay 1000000
    showBrot scr $ drop w str
    where line = take w str

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Calculate and print the uptime
uptime :: Net String
uptime = do
    now <- io getClockTime
    start <- asks startTime
    return . timeDiffToString $ diffClockTimes now start

-- Convenience
io :: IO a -> Net a
io = liftIO

