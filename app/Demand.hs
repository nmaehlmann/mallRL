module Demand where
import Apecs hiding (Map, Set)
import Pathfinding
import Item
import World



demand :: Item -> System' Float
demand i = do
    numberOfItems <- length <$> itemPositions i
    numberOfShoppinglistOccurences <- flip cfold 0 $ \x (CShoppingList sl) -> x + count i sl
    return $ (fromIntegral numberOfShoppinglistOccurences) / (fromIntegral numberOfItems)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)