import Prelude hiding (Applicative, pure, (<*>))

--------------------------------------------------------------------------------
-- Applicative

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

--------------------------------------------------------------------------------
-- Const

data Const b a = Const b deriving Show

--------------------------------------------------------------------------------
-- Exercise

instance Monoid b => Applicative (Const b) where
    pure = error "Not implemented!"

    (<*>) = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

main :: IO ()
main = print $ Const "A" <*> Const "B" <*> Const "C"
