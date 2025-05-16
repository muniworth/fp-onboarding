import Prelude hiding (Applicative, pure, (<*>), (<$>))

--------------------------------------------------------------------------------
-- Applicative

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

--------------------------------------------------------------------------------
-- Exercise

(<$>) :: Applicative f => (a -> b) -> f a -> f b
(<$>) = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

data Id a = Id a deriving Show

instance Applicative Id where
    pure = Id

    Id f <*> Id a = Id (f a)

test0 :: Id Int
test0 = (+1) <$> Id 2

main :: IO ()
main = print test0
