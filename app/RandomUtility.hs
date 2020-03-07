module RandomUtility where
import Apecs hiding (Map, Set)
import Control.Monad.Random
import World

evalRandom :: Rand StdGen a -> System' a
evalRandom g = lift $ evalRandIO g

pickRandom :: RandomGen g => [a] -> Rand g a
pickRandom l = do
    let ll = length l
    idx <- getRandomR (0, ll - 1)
    return $ l !! idx