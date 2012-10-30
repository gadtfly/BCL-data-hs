module BCL.Data where
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Network.Curl.Download
import Control.Applicative
import Control.Monad
import Data.List

-- Constants
perPage    = 1000                             :: Integer
useLocal   = True                             :: Bool     -- refactor for `useCached`?
localBase  = "cached/"                        :: FilePath
remoteBase = "http://www.bcliquorstores.com/" :: URL
productCatalogueBase = "product-catalogue"    :: Path


-- Type synonyms for readiblity
type Path = String
type URL  = String
type Page = String
type SKU  = Integer


-- Parsers: Product Catalogue
productCount :: Parser Integer
productCount = decimal <* string " Matches"

sku :: Parser SKU
sku = string "/product/" *> decimal

pageSKUs :: Parser [SKU]
pageSKUs = nub <$> many (try (skipTo sku))


-- Parsers: Utility
decimal :: Parser Integer
decimal = Token.decimal (Token.makeTokenParser Language.emptyDef)

skipTo :: Parser a -> Parser a
skipTo p = try p <|> anyChar *> skipTo p

getParse :: Parser a -> String -> a
getParse p = either (error . show) id . parse p ""


-- IO Utilities
-- Refactor with Arrows?
getLocal :: FilePath -> IO Page
getLocal = readFile

getRemote :: URL -> IO Page
getRemote = either fail return <=< openURIString

getPage :: Path -> IO Page
getPage path | useLocal  = getLocal (localBase ++ path)
             | otherwise = do
                  page <- getRemote (remoteBase ++ path)
                  writeFile (localBase ++ path) page
                  return page

parsePage :: Path -> Parser a -> IO a
parsePage path parser = getParse parser <$> getPage path


-- Main
productCatalogue :: Integer -> Integer -> Path
productCatalogue perPage page = productCatalogueBase ++ "?perPage=" ++ (show perPage) ++ "&page=" ++ (show page)

getProductCount :: IO Integer
getProductCount = parsePage (productCatalogue 1 0) (skipTo productCount)

getPageSKUs :: Integer -> IO [SKU]
getPageSKUs page = parsePage (productCatalogue perPage page) pageSKUs

getSKUs :: IO [SKU]
getSKUs = do
  count <- getProductCount
  let pages = ceiling (fromIntegral count / fromIntegral perPage)
  concat <$> mapM getPageSKUs (enumFromTo 0 (pages-1))

main :: IO ()
main = do
  print . length =<< getSKUs
  print "Success"