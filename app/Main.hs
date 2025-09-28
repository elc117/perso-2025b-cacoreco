{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.List (subsequences)

-- Define the Item data type
data Item = Item
  { itemID   :: Maybe Int
  , name     :: String
  , category :: String
  , price    :: Int
  } deriving (Show, Generic)

instance ToJSON Item
instance FromJSON Item

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field <*> field

instance ToRow Item where
  toRow (Item _ name_ category_ price_) = toRow (name_, category_, price_)

hostAny :: HostPreference
hostAny = "*"

-- Initialize database
initDB :: Connection -> IO ()
initDB conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS items (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ name TEXT,\
  \ category TEXT,\
  \ price INTEGER)"

-- Função para popular o banco de dados se estiver vazio
populateDB :: Connection -> IO ()
populateDB conn = do
  [Only count] <- query_ conn "SELECT COUNT(*) FROM items" :: IO [Only Int]
  
  if count == 0
    then do
      putStrLn "Database vazio. Adicionando items de exemplo..."
      let sampleItems =
            [ Item Nothing "Excalibur Umbra" "warframe" 120
            , Item Nothing "Kuva Bramma" "weapon" 100
            , Item Nothing "Stug" "weapon" 90
            , Item Nothing "Zephyr Prime" "warframe" 150
            , Item Nothing "Kullervo" "weapon" 80
            , Item Nothing "Dethcube Prime" "companion" 110
            , Item Nothing "Carrier Prime" "companion" 130
            , Item Nothing "Nikana Prime" "weapon" 70
            ]
      executeMany conn "INSERT INTO items (name, category, price) VALUES (?, ?, ?)" sampleItems
    else
      return ()

affordableItems :: Int -> [Item] -> [Item]
affordableItems money itemsList = filter (\item -> price item <= money) itemsList

totalPrice :: [Item] -> Int
totalPrice items = sum (map price items)

calculatePossiblePurchases :: Int -> [Item] -> [[Item]]
calculatePossiblePurchases money items =
  let
    allCombinations = subsequences items
    isValid combination = not (null combination) && totalPrice combination <= money
  in filter isValid allCombinations


-- Main entry point
main :: IO ()
main = do
  conn <- open "items.db"
  initDB conn
  populateDB conn

  mPort <- lookupEnv "PORT"
  let port = maybe 3000 id (mPort >>= readMaybe)

  putStrLn $ "Server running on port:" ++ show port
  let opts = Options
        { verbose  = 1
        , settings = setHost hostAny $ setPort port defaultSettings
        }

  scottyOpts opts $ do
    middleware logStdoutDev
    
    get "/healthz" $ text "ok"

    get "/items" $ do
      items <- liftIO $ query_ conn "SELECT id, name, category, price FROM items" :: ActionM [Item]
      json items

    get "/items/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      result  <- liftIO $ query conn "SELECT id, name, category, price FROM items WHERE id = ?" (Only idParam) :: ActionM [Item]
      if null result
        then status status404 >> json ("Item not found" :: String)
        else json (head result)

    get "/items/category/:category" $ do
      categoryParam <- pathParam "category" :: ActionM String
      items <- liftIO $ query conn "SELECT id, name, category, price FROM items WHERE category = ?" (Only categoryParam) :: ActionM [Item]
      json items

    get "/items/price/:maxPrice" $ do
      maxPriceParam <- pathParam "maxPrice" :: ActionM Int
      items <- liftIO $ query conn "SELECT id, name, category, price FROM items WHERE price <= ?" (Only maxPriceParam) :: ActionM [Item]
      json items

    get "/items/affordable/:money" $ do
      moneyParam <- pathParam "money" :: ActionM Int
      allItems <- liftIO $ query_ conn "SELECT id, name, category, price FROM items" :: ActionM [Item]
      let filteredItems = affordableItems moneyParam allItems
      json filteredItems

    get "/items/combinations/:money" $ do
      moneyParam <- pathParam "money" :: ActionM Int
      allItems <- liftIO $ query_ conn "SELECT id, name, category, price FROM items" :: ActionM [Item]
      let combinations = calculatePossiblePurchases moneyParam allItems
      json combinations
      
    post "/items" $ do
      item <- jsonData :: ActionM Item
      liftIO $ execute conn "INSERT INTO items (name, category, price) VALUES (?, ?, ?)" (name item, category item, price item)
      rowId <- liftIO $ lastInsertRowId conn
      json ("Item created with id " ++ show rowId)

    put "/items/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      item <- jsonData :: ActionM Item
      liftIO $ execute conn 
            "UPDATE items SET name = ?, category = ?, price = ? WHERE id = ?" 
            (name item, category item, price item, idParam)
      json ("Item updated" :: String)

    delete "/items/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      liftIO $ execute conn "DELETE FROM items WHERE id = ?" (Only idParam)
      json ("Item deleted" :: String)

    delete "/buy/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      
      result <- liftIO $ query conn "SELECT id, name, category, price FROM items WHERE id = ?" (Only idParam) :: ActionM [Item]

      if null result
        then do
          status status404
          json ("Item com ID " ++ show idParam ++ " não encontrado para compra." :: String)
        else do
          let item = head result
          let itemName = name item
          
          liftIO $ execute conn "DELETE FROM items WHERE id = ?" (Only idParam)
          
          json ("Item '" ++ itemName ++ "' comprado com sucesso!" :: String)