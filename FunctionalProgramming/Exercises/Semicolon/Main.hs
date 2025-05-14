import Prelude hiding (Monad, pure, (>>=), (>>))

--------------------------------------------------------------------------------
-- Monad

class Monad m where
    pure :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

--------------------------------------------------------------------------------
-- Exercise

(>>) :: Monad m => m a -> m b -> m b
(>>) = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

data M a = S a | F String deriving Show

instance Monad M where
    pure = S

    S a >>= k = k a
    F e >>= _ = F e

test0, test1, test2, test3 :: M String
test0 = S "A" >> S "B"
test1 = S "A" >> F "B"
test2 = F "A" >> S "B"
test3 = F "A" >> F "B"

main :: IO ()
main = mapM_ print [test0, test1, test2, test3]
