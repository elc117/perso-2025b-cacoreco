<div align="center">
  <h1 align="center">Backend Web com Scotty 🖥️</h1> 
  <h3 align="center">Produção Individual - Paradigmas de programação</h3>
  <p align="center">Universidade Federal de Santa Maria<br><br>Aluno: Cauã Welter da Silva | Curso: Sistemas de Informação</p>
</div>

## Objetivo 🎯

Para este trabalho, pretende-se desenvolver um serviço web que simule o funcionamento de um mercado virtual similiar ao do jogo eletrônico "Warframe", utilizando o framework Scotty. O serviço possibilitará que um usuário possa conferir uma lista de itens disponíveis para a compra, bem como consultar categorias específicas de itens e quais itens podem ser adquiridos com determinada quantidade de dinheiro. O usuário deve informar ao serviço o que deseja consultar, podendo combinar especificações de filtragem para obter exatamente a listagem de itens que deseja.

## Desenvolvimento 🧑‍💻

### Etapa 1: Entendendo servidores Web 🛜

Meu primeiro objetivo com a produção deste trabalho foi **compreender o básico do funcionamento de serviços e servidores Web**. Com pouco conhecimento sobre o assunto, esta se demonstrou uma primeira etapa importante para compreender o que está sendo produzido para esta atividade.  
O código a ser construído utilizando o framework Scotty funcionaria como um servidor Web hospedado localmente na máquina onde o código está rodando; sendo assim, a única maneira de utilizar o serviço é na própria máquina onde atualmente o código está rodando. Serviços Web como este são usualmente armazenados em grandes computadores (servidores) que guardam todo o código que o compõe. Estes servidores estão conectados à internet e podem acessados através do seu nome de domínio (DNS). No caso do nosso pequeno serviço Scotty, este pode ser acessado através do endereço **localhost:3000**, usado para **aceder a uma aplicação ou serviço web que está a ser executado no nosso próprio computador**, através da porta 3000. ***localhost*** refere-se à **sua máquina**, e ***3000*** é o **número da porta** onde o servidor de desenvolvimento está a **escutar por conexões**. O serviço também necessita de um protocolo para possibilitar a obtenção de recursos do servidor Web. Nossa aplicação Scotty utiliza o **protocolo *HTTP*** para realizar esta **comunicação entre navegador e servidor**. Essa comunicação funciona através de uma **troca de mensagens**, onde o navegador envia *requests* e o servidor retorna *responses*



### Etapa 2: Construção 🔨

Para iniciar o desenvolvimento do projeto, considerando o objetivo do trabalho, foi selecionado o código disponibilizado no material da aula que funciona como um banco de dados SQLite. Além disso, com base <a href="">neste vídeo</a>, foi criado um projeto utilizando Cabal, um sistema para construção de projetos na linguagem Haskell. Após o processo de criação, a pasta do projeto recebeu um novo arquivo .cabal e uma nova pasta "app" onde o código em Haskell principal está.

```haskell
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


-- Define the User data type
data User = User
  { userId   :: Maybe Int
  , name     :: String
  , email    :: String
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow (User _ name_ email_) = toRow (name_, email_)

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
      users <- liftIO $ query_ conn "SELECT id, name, email FROM users" :: ActionM [User]
      json users

    -- GET /users/:id
    get "/users/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      result  <- liftIO $ query conn "SELECT id, name, email FROM users WHERE id = ?" (Only idParam) :: ActionM [User]
      if null result
        then status status404 >> json ("User not found" :: String)
        else json (head result)

    -- POST /users
    post "/users" $ do
      user <- jsonData :: ActionM User
      liftIO $ execute conn "INSERT INTO users (name, email) VALUES (?, ?)" (name user, email user)
      rowId <- liftIO $ lastInsertRowId conn
      json ("User created with id " ++ show rowId)

    -- PUT /users/:id
    put "/users/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      user <- jsonData :: ActionM User
      let updatedUser = user { userId = Just idParam }
      liftIO $ execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (name updatedUser, email updatedUser, userId updatedUser)
      json ("User updated" :: String)

    -- DELETE /users/:id
    delete "/users/:id" $ do
      idParam <- pathParam "id" :: ActionM Int
      liftIO $ execute conn "DELETE FROM users WHERE id = ?" (Only idParam)
      json ("User deleted" :: String)
```

Considerando que o código possui bastante conteúdo, se demonstra necessário dividi-lo em partes menores, entender o funcionamento geral, e adicionar e remover funções conforme necessário para construção do serviço desejado.  

Primeiramente, fiz algumas alterações básicas nos dados utilizados pelo sistema. Pretende-se que cada elemento seja um item, com nome, categoria e preço. Então, o tipo de dado "User" foi alterado para "Item", e o campo "email" foi alterado para "category" (categoria). Também foi adicionado um novo campo "preço". Este último campo adicionado exige algumas outras alterações no código para garantir que tudo funcione corretamente.

```haskell
-- Define the Item data type
data Item = Item
  { itemID   :: Maybe Int
  , name     :: String
  , category    :: String
  , price :: Int
  } deriving (Show, Generic)
```

Para acomodar o campo "price", várias alterações necessitaram ser realizadas no código. por exemplo:

```haskell
instance ToRow Item where
  toRow (Item _ name_ category_ price_) = toRow (name_, category_, price_)
```

Esta função ensina a biblioteca a converter um Item em valores que podem ser usados em um pedido de INSERT ou UPDATE. Quando adicionei o campo "price" ao tipo de dado Item, o compilador me alertou que o construtor toRow exigia que o novo campo fosse adicionado à sua função.

```haskell
-- Initialize database
initDB :: Connection -> IO ()
initDB conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS items (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ name TEXT,\ 
  \ category TEXT,\
  \ price INTEGER)"
```

Para inicializar o database dos itens, também foi importante definir o campo price, especificando que este deve ser um número inteiro. Diversas outras alterações similiares foram realizadas no código, indicando às funções que existe um novo campo para cada Item. 

Durante a pesquisa, foi explicitado para mim no seguinte <a href="https://www.stackbuilders.com/insights/getting-started-with-haskell-projects-using-scotty/">tutorial</a> como é possível utilizar ferramentas em HTML para criar um front-end para a aplicação. Considerando minha experiência e interesse por criar páginas web com HTML e CSS, a opção de utilizar um front-end construído com essas duas linguagens pareceu extremamente viável. 

## Resultado

## Referências
https://youtu.be/psTTKGj9G6Y?si=NrH5j3mBDvDwkVyy - Build a Haskell Server with Scotty framework

https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Guides/Overview - Uma visão geral do HTTP

https://developer.mozilla.org/pt-BR/docs/Learn_web_development/Howto/Web_mechanics/What_is_a_web_server - O que é um servidor web (web server)?

https://www.stackbuilders.com/insights/getting-started-with-haskell-projects-using-scotty/ - Getting started with Haskell projects using Scotty 

[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/7NMOLXjY)
