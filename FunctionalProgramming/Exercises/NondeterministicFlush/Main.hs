import Prelude hiding (Monad, pure, (>>=))

--------------------------------------------------------------------------------
-- Monad

class Monad m where
    pure :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

--------------------------------------------------------------------------------
-- List

instance Monad [] where
    pure x = [x]

    [] >>= _ = []
    (x:xs) >>= k = k x ++ (xs >>= k)

die :: [a]
die = []

--------------------------------------------------------------------------------
-- Domain

data Value = Ace | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | Jack | Queen | King deriving Eq
data Suit = Spades | Hearts | Clubs | Diamonds deriving Eq
data Card = Card Value Suit deriving Eq
data Hand = Hand Card Card Card Card Card

--------------------------------------------------------------------------------
-- Exercise

flushes :: [Hand]
flushes = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

main :: IO ()
main = print $ length flushes == 617760
