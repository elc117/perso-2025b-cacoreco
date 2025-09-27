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


-- Define the Item data type
data Item = Item
  { itemID   :: Maybe Int
  , name     :: String
  , category    :: String
  , price   :: Int
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

-- Main entry point
main :: IO ()
main = do

  conn <- open "items.db"
  initDB conn

  -- pick port: env PORT (Codespaces/Render/Heroku) or default 3000
  mPort <- lookupEnv "PORT"
  let port = maybe 3000 id (mPort >>= readMaybe)

  putStrLn $ "Server running on port:" ++ show port
  let opts = Options
        { verbose  = 1
        , settings = setHost hostAny $ setPort port defaultSettings
        }

  scottyOpts opts $ do
    middleware logStdoutDev
    
    -- GET /healthz (check if the server is running)
    get "/healthz" $ text "ok"  

    -- GET /users
    get "/items" $ do
      items <- liftIO $ query_ conn "SELECT id, name, category, price FROM items" :: ActionM [Item]
      json items

    -- GET /users/:id
    get "/items/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      result  <- liftIO $ query conn "SELECT id, name, category, price FROM items WHERE id = ?" (Only idParam) :: ActionM [Item]
      if null result
        then status status404 >> json ("Item not found" :: String)
        else json (head result)

    -- POST /items
    post "/items" $ do
      item <- jsonData :: ActionM Item
      liftIO $ execute conn "INSERT INTO items (name, category, price) VALUES (?, ?, ?)" (name item, category item, price item)
      rowId <- liftIO $ lastInsertRowId conn
      json ("Item created with id " ++ show rowId)

    -- PUT /items/:id
    put "/items/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      item <- jsonData :: ActionM Item
      let updatedItem = item { itemID = Just idParam }
      liftIO $ execute conn "UPDATE items SET name = ?, category = ?, price = ?, WHERE id = ?" (name updatedItem, category updatedItem, itemID updatedItem)
      json ("Item updated" :: String)

    -- DELETE /items/:id
    delete "/items/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      liftIO $ execute conn "DELETE FROM items WHERE id = ?" (Only idParam)
      json ("Item deleted" :: String)