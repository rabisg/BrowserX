import Network.HTTP
 
 -- | 'main' runs the main program
main = do
    a <- simpleHTTP(getRequest "http://www.haskell.org/")
    b <- getResponseBody a
    print b
