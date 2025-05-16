--------------------------------------------------------------------------------
-- Monad'

class Applicative m => Monad' m where
    join :: m (m a) -> m a

--------------------------------------------------------------------------------
-- Exercise

join' :: Monad m => m (m a) -> m a
join' = error "Not implemented!"

bind' :: Monad' m => m a -> (a -> m b) -> m b
bind' = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

-- TODO

main :: IO ()
main = return ()
