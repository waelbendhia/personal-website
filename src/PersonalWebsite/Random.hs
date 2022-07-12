module PersonalWebsite.Random where

import Relude
import System.Random

newtype RandomT g a = RandomT (StateT g Identity a)
    deriving (Functor, Applicative, Monad, MonadState g)

withRandom :: g -> RandomT g a -> (a, g)
withRandom g (RandomT m) = runIdentity $ runStateT m g

randomM :: (Random a, RandomGen g) => RandomT g a
randomM = RandomT $ StateT $ Identity <$> random

randomRM :: (Random a, RandomGen g) => (a, a) -> RandomT g a
randomRM r = RandomT $ StateT $ Identity <$> randomR r
