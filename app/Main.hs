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
  } deriving (Show, Generic)

instance ToJSON Item
instance FromJSON Item

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field

instance ToRow Item where
  toRow (Item _ name_ category_) = toRow (name_, category_)

hostAny :: HostPreference
hostAny = "*"

-- Initialize database
initDB :: Connection -> IO ()
initDB conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS users (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ name TEXT,\ 
  \ email TEXT)"

-- Main entry point
main :: IO ()
main = do

  conn <- open "users.db"
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
    get "/users" $ do
      users <- liftIO $ query_ conn "SELECT id, name, email FROM users" :: ActionM [Item]
      json users

    -- GET /users/:id
    get "/users/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      result  <- liftIO $ query conn "SELECT id, name, email FROM users WHERE id = ?" (Only idParam) :: ActionM [Item]
      if null result
        then status status404 >> json ("Item not found" :: String)
        else json (head result)

    -- POST /users
    post "/users" $ do
      user <- jsonData :: ActionM Item
      liftIO $ execute conn "INSERT INTO users (name, email) VALUES (?, ?)" (name user, category user)
      rowId <- liftIO $ lastInsertRowId conn
      json ("Item created with id " ++ show rowId)

    -- PUT /users/:id
    put "/users/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      user <- jsonData :: ActionM Item
      let updatedUser = user { itemID = Just idParam }
      liftIO $ execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (name updatedUser, category updatedUser, itemID updatedUser)
      json ("Item updated" :: String)

    -- DELETE /users/:id
    delete "/users/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      liftIO $ execute conn "DELETE FROM users WHERE id = ?" (Only idParam)
      json ("Item deleted" :: String)