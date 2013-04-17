module BrowserX.Db(put_cookieDB,get_cookies,addCookies) where


import Database.HDBC.Sqlite3
import Database.HDBC
import Network.Browser

import Network.HTTP
import Network.Browser


   
put_cookieDB [] = return ()
put_cookieDB (cookie:list) = do
	con <- connectSqlite3 "test.db"
	let a = case ckPath cookie of 
		Nothing -> "Nothing"
		Just path -> path
	let b = case ckComment cookie of 
		Nothing -> "Nothing"
		Just comment -> comment
	let c = case ckVersion cookie of 
		Nothing -> "Nothing"
		Just version -> version 
	g <- run con "CREATE TABLE IF NOT EXISTS cookies (name VARCHAR(1000),  domain VARCHAR(1000), value VARCHAR(1000), path VARCHAR(1000), comment VARCHAR(1000), version VARCHAR(1000))" []
	h <- run con "INSERT INTO cookies values ( ?, ?, ?, ?, ?, ?)" [toSql (ckName cookie::String), toSql (ckDomain cookie::String), toSql (ckValue cookie::String), toSql (c::String), toSql (b::String), toSql (c::String)] 
	commit con
	disconnect con
	put_cookieDB list
 

get_cookies cookie= do
	con <- connectSqlite3 "test.db"
	r <- quickQuery con "SELECT * from cookies" []  		
	disconnect con
	return $ get_cookie_format r

get_cookie_format :: [[SqlValue]] -> [Cookie]
get_cookie_format [] = [] 
get_cookie_format (x:xs) = case x of 
		[name, domain, value, path, comment, version] -> (MkCookie {ckName = (fromSql name)::String,ckDomain = (fromSql domain)::String,ckValue = (fromSql value)::String,ckPath = maybe_path::Maybe String,ckComment = maybe_comment::Maybe String,ckVersion = maybe_version::Maybe String}:(get_cookie_format xs))
					where maybe_path = case fromSql path of 
								"Nothing" -> Nothing
								x -> Just x
					      maybe_comment = case fromSql comment of 
								"Nothing" -> Nothing
								x -> Just x
					      maybe_version = case fromSql version of 
								"Nothing" -> Nothing
								x -> Just x
				  	
	
addCookies [] = return ()
addCookies (cookie:list) = do
    addCookie cookie
    addCookies list
