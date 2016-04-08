module Main where

import Lib
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
    Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    putStrLn "Finished node establishment"
