{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Text.Printf

step1 :: IO (Int, Int)
step1 = do
	conn <- connect defaultConnectInfo
	[(i, j)] <- query_ conn "select 2+2, 2+3"
	return (i, j)

step2 :: IO (String, String)
step2 = do
	conn <- connect defaultConnectInfo { connectDatabase = "mysql" }
	(username, hostname):_ <- query_ conn "select User, Host from user"
	return (username, hostname)

step3 :: IO ()
step3 = do
	conn <- connect defaultConnectInfo { connectDatabase = "mysql" }
	results <- query_ conn "select User, Host from user"
	printf "Got back %s\n" (show (results :: [(String,String)]))

step4 :: IO (Int)
step4 = do
	conn <- connect defaultConnectInfo { connectDatabase = "mysql" }
	results <- query_ conn "select User, Host from user"
	mapM_ (\(username, hostname) -> printf "%s only logs in from %s\n" username hostname) (results :: [(String,String)])
	return $ length results

main = step1
