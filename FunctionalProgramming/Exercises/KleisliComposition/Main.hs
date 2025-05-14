import Prelude hiding (Monad, pure, (>>=), (>>))

--------------------------------------------------------------------------------
-- Monad

class Monad m where
    pure :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

--------------------------------------------------------------------------------
-- Exercise

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

data M a = S a | F String deriving Show

instance Monad M where
    pure = S

    S a >>= k = k a
    F e >>= _ = F e

test0, test1, test2, test3 :: M String
test0 = (S >=> S) "A"
test1 = (S >=> F) "A"
test2 = (F >=> S) "A"
test3 = (F >=> F) "A"

main :: IO ()
main = mapM_ print [test0, test1, test2, test3]
