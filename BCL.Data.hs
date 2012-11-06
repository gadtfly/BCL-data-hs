
module BCL.Data where
import qualified Control.Exception as Exception
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Network.Curl.Download
import Control.Applicative
import Control.Monad
import Data.List
import Debug.Trace

-- Constants

perPage       = 1000                              :: Integer
useCached     = True                              :: Bool
cachePath     = "cached/"                         :: String
urlPath       = "http://www.bcliquorstores.com/"  :: String
cataloguePath = "product-catalogue"               :: String
productPath   = "product/"                        :: String



-- Product Details

data Product = Product



-- Parsers: Product Catalogue

matches :: Parser Integer
matches = decimal <* string " Matches"

sku :: Parser Integer
sku = string "/product/" *> decimal

pageSKUs :: Parser [Integer]
pageSKUs = nub <$> many (try (skipTo sku))



-- Parsers: Utility

decimal :: Parser Integer
decimal = Token.decimal (Token.makeTokenParser Language.emptyDef)

skipTo :: Parser a -> Parser a
skipTo p = try p <|> anyToken *> skipTo p



-- Main Utilities

catalogueQuery :: Integer -> Integer -> String
catalogueQuery x n = cataloguePath ++ "?perPage=" ++ show x ++ "&page=" ++ show n

productQuery :: Integer -> String
productQuery n = productPath ++ show n

getCached :: String -> IO String
getCached path = readFile (cachePath ++ path)

updateCached :: String -> String -> IO ()
updateCached path page = writeFile (cachePath ++ path) page

getUrl :: String -> IO String
getUrl path = either fail return =<< openURIString (urlPath ++ path)

getPage :: String -> IO String
getPage path  | useCached = getCached path
              | otherwise = do
                  page <- getUrl path
                  updateCached path page
                  return page

handler :: a -> Exception.SomeException -> IO a
handler x e = traceShow e (return x)


-- Main

getMatches :: IO Integer
getMatches = do
  page <- getPage (catalogueQuery 1 0)
  return $ either (error . show) id $ parse (skipTo matches) "getMatches" page

getPageSKUs :: String -> IO [Integer]
getPageSKUs path = do
  page <- Exception.catch (getPage path) (handler "")
  return $ either (error . (++ ": " ++ path) . show) id $ parse pageSKUs ("getPageSKUs" ++ path) page

getSKUs :: Integer -> IO [Integer]
getSKUs matches = do
  let pageTotal   = ceiling $ fromIntegral matches / fromIntegral perPage
      pageNumbers = enumFromTo 0 (pageTotal - 1)
      paths       = map (catalogueQuery perPage) pageNumbers
  concat <$> mapM getPageSKUs paths

getProduct :: Integer -> IO Product
getProduct sku = do
  page <- Exception.catch (getPage (productQuery sku)) (handler "")
  return Product

getProducts :: [Integer] -> IO [Product]
getProducts skus = do
  mapM getProduct skus



main = do
  putStrLn "Starting"
  matches <- getMatches
  putStrLn $ "Matches: " ++ show matches
  skus <- getSKUs matches
  putStrLn $ "SKUs found: " ++ show (length skus)
  products <- getProducts skus
  putStrLn $ "Products found: " ++ show (length products)
  putStrLn "Success"