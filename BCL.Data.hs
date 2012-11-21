{-# LANGUAGE OverloadedStrings #-}

module BCL.Data where
import Prelude hiding (writeFile, readFile)
import Data.ByteString.Lazy.Char8 hiding (map)
import Network.Curl.Download.Lazy
import Control.Applicative
import System.Directory
import System.FilePath
import Data.Aeson


remotePath    = "http://www.bcliquorstores.com/app" :: FilePath
localPath     = "cached"      :: FilePath
productsPath  = "/products"   :: FilePath
storesPath    = "/stores"     :: FilePath
inventoryPath = "/inventory"  :: FilePath


createFile :: FilePath -> ByteString -> IO ()
createFile path s = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile (path ++ ".json") s

getRemote :: FilePath -> IO ByteString
getRemote path = do
  contents <- either (error path) id <$> openLazyURI (remotePath ++ path)
  createFile (localPath ++ path) contents
  return contents


data Products = Products  { products  :: [Product]  } deriving Show
data Product  = Product   { serial0   :: Serial     } deriving Show
data Serial   = Serial    { serial1   :: String     } deriving Show
instance FromJSON Products where
  parseJSON (Object x) = do
    products <- parseJSON =<< (x .: "products")
    return $ Products products

instance FromJSON Product where
  parseJSON (Object x) = Product <$> x .: "product"

instance FromJSON Serial where
  parseJSON (Object x) = Serial <$> x .: "Serial"


main :: IO ()
main = do
  json <- getRemote productsPath
  let (Just products') = (decode json :: Maybe Products)
  let serials = map (serial1 . serial0) (products products')
  mapM_ (\serial -> getRemote (productsPath ++ "/" ++ serial ++ inventoryPath)) serials
  print "done"