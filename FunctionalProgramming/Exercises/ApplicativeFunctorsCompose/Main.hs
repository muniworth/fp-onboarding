import Prelude hiding (Applicative, pure, (<*>), fail)

--------------------------------------------------------------------------------
-- Applicative

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

--------------------------------------------------------------------------------
-- Compose

data Compose f g a = Compose (f (g a)) deriving Show

--------------------------------------------------------------------------------
-- Exercise

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = error "Not implemented!"

    (<*>) = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

data V e a = S a | F e deriving Show

instance Semigroup e => Applicative (V e) where
    pure = S

    S f <*> S a  = S (f a)
    S _ <*> F e  = F e
    F e <*> S _  = F e
    F e <*> F e' = F (e <> e')

purefail :: String -> Compose (V [String]) (V [String]) a
purefail = Compose . pure . F . return

fail :: String -> Compose (V [String]) (V [String]) a
fail = Compose . F . return

test0, test1, test2, test3, test4, test5, test6, test7, test8 :: Compose (V [String]) (V [String]) String
test0 = pure (++) <*> pure "A"     <*> pure "B"
test1 = pure (++) <*> pure "A"     <*> purefail "B"
test2 = pure (++) <*> pure "A"     <*> fail "B"
test3 = pure (++) <*> purefail "A" <*> pure "B"
test4 = pure (++) <*> purefail "A" <*> purefail "B"
test5 = pure (++) <*> purefail "A" <*> fail "B"
test6 = pure (++) <*> fail "A"     <*> pure "B"
test7 = pure (++) <*> fail "A"     <*> purefail "B"
test8 = pure (++) <*> fail "A"     <*> fail "B"

main :: IO ()
main = mapM_ print [test0, test1, test2, test3, test4, test5, test6, test7, test8]
