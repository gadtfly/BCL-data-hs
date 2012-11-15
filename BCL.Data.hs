
module BCL.Data where
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Network.Curl.Download
import Control.Applicative
import Control.Error
import Control.Proxy
import Data.List

-- Constants

perPage       = 1000                              :: Integer
useLocal      = True                              :: Bool
localPath     = "cached/"                         :: String
remotePath    = "http://www.bcliquorstores.com/"  :: String
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



-- Path builders

catalogueQuery :: Integer -> Integer -> String
catalogueQuery x n = cataloguePath ++ "?perPage=" ++ show x ++ "&page=" ++ show n

productQuery :: Integer -> String
productQuery n = productPath ++ show n



-- Main Utilities

putLocal :: String -> String -> Script ()
putLocal path = scriptIO . writeFile (localPath ++ path)

getLocal :: String -> Script String
getLocal path = scriptIO $ readFile (localPath ++ path)

getRemote :: String -> Script String
getRemote path = EitherT $ openURIString (remotePath ++ path)

getPage :: String -> Script String
getPage path  | useLocal = getLocal path
              | otherwise = do
                  page <- getRemote path
                  putLocal path page
                  return page

getParse :: Parser a -> String -> Script a
getParse parser input = hoistEither $ fmapL show $ parse parser "" input

withDefault :: a -> Script a -> Script a
withDefault x = handleT handler
  where
    handler e = scriptIO (errLn e) >> return x



-- Main

getMatches :: Script Integer
getMatches = getPage (catalogueQuery 1 0) >>= getParse (skipTo matches)

getPageSKUs :: String -> Script [Integer]
getPageSKUs path = withDefault [] $ getPage path >>= getParse pageSKUs

getSKUs :: Integer -> Script [Integer]
getSKUs matches = concat <$> mapM getPageSKUs paths
  where
    totalPages  = ceiling $ fromIntegral matches / fromIntegral perPage
    pageNumbers = enumFromTo 0 $ totalPages - 1
    paths       = map (catalogueQuery perPage) pageNumbers

--getProducts :: [Integer] -> Script [()]
--getProducts skus = mapM (getPage . productQuery) skus

main :: IO ()
main = do
  errLn "Starting"
  matches  <- runScript $ getMatches
  errLn $ "Matches: " ++ show matches
  skus     <- runScript $ getSKUs matches
  errLn $ "SKUs: " ++ show (length skus)
  --products <- runScript $ getProducts skus
  --errLn $ "Products: " ++ show (length products)
  errLn "Success"