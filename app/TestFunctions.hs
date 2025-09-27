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