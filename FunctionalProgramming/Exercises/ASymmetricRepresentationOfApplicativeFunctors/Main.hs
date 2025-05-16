--------------------------------------------------------------------------------
-- Applicative'

class Functor f => Applicative' f where
    unit :: f ()
    cross :: f a -> f b -> f (a, b)

--------------------------------------------------------------------------------
-- Exercise

unit' :: Applicative f => f ()
unit' = error "Not implemented!"

cross' :: Applicative f => f a -> f b -> f (a, b)
cross' = error "Not implemented!"

pure' :: Applicative' f => a -> f a
pure' = error "Not implemented!"

ap' :: Applicative' f => f (a -> b) -> f a -> f b
ap' = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

-- TODO

main :: IO ()
main = return ()
