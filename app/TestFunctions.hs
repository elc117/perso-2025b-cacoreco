import Data.List (subsequences)

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
novaArmaKuvaBramma = Item Nothing "Kuva Bramma" "Weapon" 60

affordableItems :: Int -> [Item]
affordableItems money = filter (\item -> price item <= money) items
  where
    items = [novoWarframeMesa, novoWarframeExcalibur, novaArmaKuvaBramma]
    item = head items

totalPrice :: [Item] -> Int
totalPrice items = sum (map price items)

calculatePossiblePurchases :: Int -> [Item] -> [[Item]]
calculatePossiblePurchases money items =
  let
    allCombinations = subsequences items
    
    isValid combination = not (null combination) && totalPrice combination <= money
  
  in filter isValid allCombinations

main :: IO ()
main = do
  let budget = 190
  let availableItems = affordableItems budget
  putStrLn $ "Com uma quantidade de " ++ show budget ++ " platinas, você pode comprar:"
  mapM_ (putStrLn . name) availableItems

  let possiblePurchases = calculatePossiblePurchases budget availableItems
  putStrLn "\nCombinações possíveis de compras dentro do orçamento:"
  mapM_ (putStrLn . show . map name) possiblePurchases