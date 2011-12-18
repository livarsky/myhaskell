module Main where

import Network
import System.IO
import System.Environment
import Control.Concurrent
import Control.Monad
import Directory

main :: IO ()
main = withSocketsDo $ do
    [port] <- getArgs
    sock <- listenOn (PortNumber $ fromIntegral (read port :: Int))
    forever $ do
        (handle, _, _) <- accept sock
        forkIO $ do
            hSetBuffering handle LineBuffering
            request <- hGetLine handle
            hSetBuffering handle NoBuffering
            response request >>= hPutStr handle
            hClose handle

response :: String -> IO String
response request = do
	if take 5 request == "GET /"
	then do
		fname <- return $ drop 5 $ take (length request - 10) request
		exists <- doesFileExist fname
		if exists
		then do
			fhandle <- openFile fname ReadMode
			fcontents <- hGetContents fhandle
			seq fcontents $ hClose fhandle
			return $ "HTTP/1.1 200 OK\r\nContent-Length: " ++ (show (length fcontents)) ++ "\r\n\r\n" ++ fcontents
		else return "HTTP/1.1 404 Not Found\r\n\r\n"
	else return "HTTP/1.1 400 Bad Request\r\n\r\n"
