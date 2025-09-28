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

Aqui será feita uma análise de seções de código comum para todas as aplicações do framework Scotty. Estes trechos exploram o funcionamento do servidor, demonstrando requisições e a comunicação através da porta 3000. 

```haskell
  -- pick port: env PORT (Codespaces/Render/Heroku) or default 3000
  mPort <- lookupEnv "PORT"
  let port = maybe 3000 id (mPort >>= readMaybe)
```

Nesse trecho de código, o programa configura a porta do servidor, verificando se a variável de ambiente "PORT" está definida. Caso contrário, utiliza a porta padrão 3000.

```haskell
    -- GET /users
    get "/items" $ do
      items <- liftIO $ query_ conn "SELECT id, name, category, price FROM items" :: ActionM [Item]
      json items
```

Esta seção de código explora o funcionamento de uma requisição do tipo GET pela rota /items. Aqui, quero demonstrar apenas o acesso de rotas. Essa função get será explicada posteriormente.   

### Etapa 2: Construção 🔨

Para iniciar o desenvolvimento do projeto, considerando o objetivo principal do trabalho, foi selecionado o código disponibilizado no material da aula que funciona como um banco de dados SQLite. Além disso, com base <a href="">neste vídeo</a>, foi criado um projeto utilizando Cabal, um sistema para construção de projetos na linguagem Haskell. Após o processo de criação, a pasta do projeto recebeu um novo arquivo .cabal e uma nova pasta "app" onde o código em Haskell principal está.

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

Levando em consideração que o código possui bastante conteúdo, se demonstra necessário dividi-lo em partes menores, entender o funcionamento geral, e adicionar e remover funções conforme necessário para construção do serviço desejado.  

- Primeiramente, fiz algumas alterações básicas nos dados utilizados pelo sistema. Pretende-se que cada elemento seja um item, com nome, categoria e preço. Então, o tipo de dado "User" foi alterado para "Item", e o campo "email" foi alterado para "category" (categoria). Também foi adicionado um novo campo "preço". Este último campo adicionado exige algumas outras alterações no código para garantir que tudo funcione corretamente.

```haskell
-- Define the Item data type
data Item = Item
  { itemID   :: Maybe Int
  , name     :: String
  , category    :: String
  , price :: Int
  } deriving (Show, Generic)
```

- Para acomodar o campo "price", várias alterações necessitaram ser realizadas no código. por exemplo:

```haskell
instance ToRow Item where
  toRow (Item _ name_ category_ price_) = toRow (name_, category_, price_)
```

- Esta função ensina a biblioteca a converter um Item em valores que podem ser usados em um pedido de INSERT ou UPDATE. Quando adicionei o campo "price" ao tipo de dado Item, o compilador me alertou que o construtor toRow exigia que o novo campo fosse adicionado à sua função.

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

- Para inicializar o database dos itens, também foi importante definir o campo price, especificando que este deve ser um número inteiro. Diversas outras alterações similiares foram realizadas no código, indicando às funções que existe um novo campo para cada Item.  

A próxima etapa na construção do código é implementar as funções para filtrar itens (por categoria, preço, ou ambos) e para calcular quais itens podem ser adquiridos com uma quantidade específica de dinheiro informada pelo usuário. Para isto, é preciso entender como as requisições funcionam dentro do Scotty. Serão usadas como exemplo as novas requisições adicionadas ao código

```haskell
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

    -- GET /items/category/:category
    get "/items/:category" $ do
      categoryParam <- pathParam "category" :: ActionM String
      items <- liftIO $ query conn "SELECT id, name, category, price FROM items WHERE category = ?" (Only categoryParam) :: ActionM [Item]
      json items

    get "/items/price/:maxPrice" $ do
      maxPriceParam <- pathParam "maxPrice" :: ActionM Int
      items <- liftIO $ query conn "SELECT id, name, category, price FROM items WHERE price <= ?" (Only maxPriceParam) :: ActionM [Item]
      json items
```
- As requisições demonstradas acima são todas do tipo GET, o que significa que, quando recebidas pelo servidor, este executa uma consulta ao banco de dados para buscar os registros da tabela "items", convertendo a tabela para formato JSON e enviando esta nova tabela para o usuário (Nesse caso, o navegador web). 
  - Mais detalhadamente: get funciona como uma função, que recebe como argumento os caminhos da URL (como o bloco de código acima demonstra, este caminho pode ser ```/items```, ```/items/:category...```). 
  - A linha de código```items <- liftIO $ query_ conn "SELECT id, name, category, price FROM items" :: ActionM [Item]```, presente na requisição mais simples (```get /items```) aparece de maneira similiar nas requisições mais complexas também. Essa é uma string de consulta SQL padrão. Ela instrui o banco de dados a selecionar as colunas id, name, category e price de todos os registros na tabela items.
  - ```query_ conn "..."``` é uma função da biblioteca ```Database.SQLite.Simple.``` O ```_``` no final do nome ```(query_)``` indica que a consulta não possui parâmetros (entretanto, uma consulta com ```WHERE id = ?``` deve explicitar parâmetros para ```query```. No código, diversas requisisões desse tipo foram implementadas). Ela recebe a conexão com o banco ```(conn)``` e a string SQL, executa a consulta e retorna o resultado. Uma interação com um banco de dados é uma operação de I/O (Entrada/Saída), que pode ter efeitos colaterais. Em Haskell, essas operações são encapsuladas na mônada IO, portanto, o tipo de retorno de ```query_``` é ```IO [Item]```, ou seja, é uma ação de I/O que, quando executada, produzirá uma lista de Itens.
  - ```liftIO $ ...``` esta função eleva uma ação IO para dentro de uma mônada compatível (no nosso caso, ```ActionM```). Assim, ```liftIO $ query_ ...``` transforma a ação ```IO [Item]``` em uma ação ```ActionM [Item]```
  - A sintaxe de ```items <- ...:``` atribui o resultado extraído de ```liftIO $ query_ ...(a lista de itens, do tipo [Item])``` à variável ```items```. A partir deste ponto, ```items``` funciona como uma lista Haskell comum.
  - ```ActionM :: [Item]``` informa ao compilador Haskell (e a quem está lendo o código) que o resultado esperado de toda a expressão à direita é uma ```ActionM [Item]```, ajudando a ```query_``` a inferir que ela precisa converter as linhas do banco de dados em registros do tipo Item, usando a instância ```FromRow Item```.
  - ```json items```converte a lista ```items``` para uma string no formato JSON, enviando essa string JSON de volta ao cliente como o corpo da resposta HTTP, configurando automaticamente o cabeçalho ```Content-Type``` para ```application/json```.

- As requisições mais complexas seguem uma estrutura similiar, com a maior diferença sendo a especificação de parâmetros de filtragem para a lista de items extraída do database. Para a sua implementação, não foram encontrados muitos erros, considerando que somente foi necessário algumas pequenas alterações em funções já existentes. Entretanto, foi crucial analisar com cuidado a linha ```items <- liftIO $ query conn "SELECT id, name, category, price FROM items WHERE price <= ?" (Only maxPriceParam) :: ActionM [Item]``` para implementar a funcionalidade necessária, considerando que esta é uma string SQL, então sua sintaxe é diferente do usual. Foi importante entender o funcionamento de ```WHERE price <= ?``` considerando que essa é a função crucial para realizar a filtragem desejada, servindo como uma simples comparação de menor ou igual. O ```?``` age como uma variável, pois é substituído pelo número inteiro inserido pelo usúario no endereço.

```haskell
-- Função para popular o banco de dados se estiver vazio
populateDB :: Connection -> IO ()
populateDB conn = do
  -- Verifica quantos items já existem
  [Only count] <- query_ conn "SELECT COUNT(*) FROM items" :: IO [Only Int]
  
  if count == 0
    then do
      putStrLn "Database vazio. Adicionando items de exemplo..."
      -- Lista de items para inserir
      let sampleItems =
            [ Item Nothing "Excalibur Umbra" "warframe" 120
            , Item Nothing "Kuva Bramma" "weapon" 100
            , Item Nothing "Stug" "weapon" 90
            , Item Nothing "Zephyr Prime" "warframe" 150
            ]
      -- Usa executeMany para inserir todos de uma vez
      executeMany conn "INSERT INTO items (name, category, price) VALUES (?, ?, ?)" sampleItems
    else
      return () -- Se não estiver vazio, não faz nada
```

- Foi adicionada também uma função para popular automaticamente o banco de dados com alguns items de exemplo caso o database estiver vazio (evitando a necessidade de utilizar ```curl``` no terminal para adicionar novos items a fim de testes). 
  - A função conta quantos items já existem dentro do database ```[Only count] <- query_ conn "SELECT COUNT(*) FROM items" :: IO [Only Int]```. Caso a contagem resultar em 0 (database vazio), o programa insere alguns items de exemplo, primeiro adicionado-os em uma lista sampleItems, especificando nome, categoria e preço (o ID do item é selecionado pelo programa). Logo após, através da linha ```executeMany conn "INSERT INTO items (name, category, price) VALUES (?, ?, ?)" sampleItems``` (string padrão que se comunica com o SQL), os items são adicionados todos de uma vez ao database. 

A próxima etapa é adicionar funções que filtrem a lista de acordo com a quantidade de dinheiro especificada pelo usuário. Para isso, serão desenvolvidas funções em Haskell puro, que funcionariam sem a necessidade do Scotty.
- A função para filtrar a lista de acordo com os itens que um usuário pode adquirir deve, principalmente, comparar o preço de cada item com o dinheiro especificado pelo usuário. Em um arquivo teste, montei a seguinte função, em Haskell puro:

```haskell
data Item = Item
  { itemID   :: Maybe Int
  , name     :: String
  , category    :: String
  , price   :: Int
  }deriving (Show)

novoWarframeMesa :: Item
novoWarframeMesa = Item Nothing "Mesa Prime" "Character" 150

novoWarframeExcalibur :: Item
novoWarframeExcalibur = Item Nothing "Excalibur Umbra" "Character" 120

novaArmaKuvaBramma :: Item
novaArmaKuvaBramma = Item Nothing "Kuva Bramma" "Weapon" 100

affordableItems :: Int -> [Item]
affordableItems money = filter (\item -> price item <= money) items
  where
    items = [novoWarframeMesa, novoWarframeExcalibur, novaArmaKuvaBramma]
    item = head items

main :: IO ()
main = do
  let budget = 130
  let availableItems = affordableItems budget
  putStrLn $ "Com uma quantidade de " ++ show budget ++ " platinas, você pode comprar:"
  mapM_ (putStrLn . name) availableItems
```
- Um desafio foi compreender como realizar operações com tipos de dado Item, embora a solução tenha sido mais simples do que o esperado. Para a função affordableItems, primeiro tentei:

```haskell
affordableItems :: Int -> [Item]
affordableItems money = filter (\items <= money) items
  where items = [Item]
```
- O que não funciona pois interpretei a estrutura `Ìtem` não como um tipo de dado, mas sim como uma lista (similiar ao que acontecia na linguagem C, onde uma `struct` era depois definida como um tipo de dado e este era utilizado nas listas encadeadas). O campo price também não está especificado no campo da comparação, e `items` é uma variável que já possui (ou deveria possuir) a lista de items, não agindo como uma variável para iterar na lista a função filter (essa é a função desejada).

```haskell
affordableItems :: Int -> [Item]
affordableItems money = filter (\price items <= money) items
  where
    items = [novoWarframeMesa, novoWarframeExcalibur, novaArmaKuvaBramma]
```
- Após, cheguei próximo à solução final, entretanto, `items` ainda está sendo utilizada da maneira incorreta, e o campo price não seria lido corretamente. Finalmente, foi necessário definir uma nova variável `item` que assume o valor da `head` da lista de itens, assim iterando pela lista através da função filter.

```haskell
affordableItems :: Int -> [Item] -> [Item]
affordableItems money itemsList = filter (\item -> price item <= money) itemsList
```
- A função final, quando implementada no código principal, ficou assim. Alterações foram feitas para deixar o código mais simples, eliminando as variáveis auxiliares `item`e `items`, e fazendo uso da variável itemsList (já definida anteriormente no código) que possui a lista dos itens do database.

Também implementei funções que permitem ao usuário conferir possíveis combinações de itens de acordo com seu dinheiro. 

```haskell
totalPrice :: [Item] -> Int
totalPrice items = sum (map price items)

calculatePossiblePurchases :: Int -> [Item] -> [[Item]]
calculatePossiblePurchases money items =
  let
    allCombinations = subsequences items
    
    isValid combination = not (null combination) && totalPrice combination <= money
  
  in filter isValid allCombinations
```
- Duas funções distintas foram adicionadas ao código. A função `totalPrice`é uma função simples que recebe uma lista de itens e calcula a soma de seus preços.
- `calculatePossiblePurchases :: Int -> [Item] -> [[Item]] é a função principal`. 
  - Ela utiliza da função `subsequences`, importada de `Data.List`, que gera todas as combinações possíveis de uma lista Por exemplo, subsequences `[A, B]` retorna `[[],[A],[B],[A,B]]`.
  - `isValid combination` é outra função local que verifica se uma combinação é válida. A combinação precisa atender duas condições: Não ser vazia e a soma de seus preços não ser maior do que o dinheiro informado pelo usuário.
  - `filter isValid allCombinations` aplica `isValid` como filtro para o resultado da função allCombinations`.

- Para criar essa função, diversos problemas tiveram que ser solucionados. Para adaptar várias funcionalidades, algumas sugestões do Github Copilot foram utilizadas. Antes de aprender sobre a função `subsequences`, foi extremamente difícil pensar em alguma solução para gerar combinação de listas. O restante da função é razoavelmente simples, então não surgiram muitos problemas durante o desenvolvimento 

```haskell
findCombinations :: [Item] -> [[Item]]
findCombinations [] = [[]]
findCombinations (item:xs) =
  let
    combinationsWithoutItem = findCombinations xs
    combinationsWithItem = map (item:) (findCombinations xs)

calculatePossiblePurchases :: Int -> [Item] -> [[Item]]
calculatePossiblePurchases money items =
  let
    allCombinations = findCombinations items
```
  - Uma solução possível foi pensada aqui, entretanto, ela não apresenta o funcionamento correto. Ela deveria gerar todas as combinações que incluem `item` e todas que não o incluem recursivamente, e depois juntaria as duas. Entretanto, a expressão findCombinations xs é executada duas vezes. Isso significa que para cada passo da recursão, a função recalcula toda a lista de soluções para a sub-lista xs duas vezes, em vez de calcular uma vez e reutilizar o resultado para as outras recursões. Isso acaba gerando problemas de memória, e o programa congela dependendo do número de itens. Embora essa função talvez pudesse ser reaproveitada e implementada de maneira diferente, optei por buscar metódos alternativos para deixar a solução mais simples e evitar frustrações.

Também adicionei uma nova rota `buy` que possibilita a "compra" de itens pelo terminal, utilizando `curl`

```haskell
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
```
- A funcionalidade do código é muito similiar à requisição `delete` padrão, com algumas diferenças:
  - O programa obtém o item especificado por meio do ID e coloca o resultado na variável `result`
  - O programa confere se o item com o ID inserido existe ou não. Caso não exista, uma mensagem de erro aparece para o usuário
  - Caso o ID realmente exista, o item é deletado normalmente, e uma mensagem de sucesso aparece para o usuário.
- Sendo uma função simples de implementar, não surgiram grandes problemas durante sua construção.

## Resultado ✅

Para a execução do código final, é importante criar uma pasta com:
  - O arquivo `.cabal`, que possui todas as configurações do projeto.
  - Uma segunda pasta chamada `app`, com o arquivo `Main.hs` dentro. 
O arquivo `.cabal` cuida de todas as depêndecias necessárias (Como, por exemplo, o próprio framework Scotty). Também é possível instalar o Scotty globalmente na máquina.

Os itens do database são:

```haskell
[ Item Nothing "Excalibur Umbra" "warframe" 120
  , Item Nothing "Kuva Bramma" "weapon" 100
  , Item Nothing "Stug" "weapon" 90
  , Item Nothing "Zephyr Prime" "warframe" 150
  , Item Nothing "Kullervo" "weapon" 80
  , Item Nothing "Dethcube Prime" "companion" 110
  , Item Nothing "Carrier Prime" "companion" 130
  , Item Nothing "Nikana Prime" "weapon" 70
]
```
- Você pode pesquisar por categoria (warframe, weapon ou companion) com: `get "/items/category/:category"`
- Pesquisar por preço, podendo utilizar as funções que: 
  - Filtram itens pelo preço: `get "/items/price/:maxPrice"` ou `/items/affordable/:money` 
  - Ou a função que  mostra as combinações dos itens disponíveis para compra com seu dinheiro: `get "/items/combinations/:money"`
- Mostrar uma lista com todos os itens: `get "/items"`
  - Ou pesquisar cada item por seu ID: `get "/items/:id"`

Caso esteja utilizando um navegador web, substitua o `get` por `localhost:3000`.  
Lembre-se de substituir nomes após `:` como em `/:category` pelo nome correspondente associado ao item desejado! 

A execucação do projeto final ficou assim:

https://github.com/user-attachments/assets/a40dd73c-e5b6-43e5-87a4-41fc5a03461e

## Referências 📚
https://youtu.be/psTTKGj9G6Y?si=NrH5j3mBDvDwkVyy - Build a Haskell Server with Scotty framework

https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Guides/Overview - Uma visão geral do HTTP

https://developer.mozilla.org/pt-BR/docs/Learn_web_development/Howto/Web_mechanics/What_is_a_web_server - O que é um servidor web (web server)?

https://www.stackbuilders.com/insights/getting-started-with-haskell-projects-using-scotty/ - Getting started with Haskell projects using Scotty 

https://hackage.haskell.org/package/scotty-0.22/docs/Web-Scotty.html - Web.Scotty

https://g.co/gemini/share/704e10cdd2e5 - Prompt Gemini : Popular Banco de Dados

https://help.interfaceware.com/kb/1062/2 - db conn query{}: Reading from a database

https://book.realworldhaskell.org/read/data-structures.html - Chapter 13. Data Structures

https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html - Data List

https://www.w3schools.com/js/js_json.asp - JavaScript JSON


[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/7NMOLXjY)
