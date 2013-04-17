import Data.Char
import Database.HDBC.Sqlite3
import Database.HDBC
import Network.HTTP
import Network.Browser

main = do
    (rsp, cookies) <- browse $ do
        rsp <- request $ getRequest "http://google.com/"
        cookies <- getCookies
        return (rsp, cookies)
    put_cookieDB cookies
    case cookies of 
		(x:xs) -> do get_cookie x 

    

put_cookieDB [] = return ()
put_cookieDB (cookie:list) = do
	con <- connectSqlite3 "test.db"
	a <- run con "CREATE TABLE IF NOT EXISTS cookies (name VARCHAR(100),  domain VARCHAR(1000), value VARCHAR(1000))" []
	b <- run con "INSERT INTO cookies values ( ?, ?, ?)" [toSql (ckName cookie::String), toSql (ckDomain cookie::String), toSql (ckValue cookie::String)]
	commit con
	r <- quickQuery con "SELECT * from cookies" []
	disconnect con
	put_cookieDB list
 

get_cookie cookie = do
	putStrLn $ ckName cookie
	con <- connectSqlite3 "test.db"
	r <- quickQuery con "SELECT * from cookies where name = ?" [toSql (ckDomain cookie::String)]
	print r
	disconnect con
