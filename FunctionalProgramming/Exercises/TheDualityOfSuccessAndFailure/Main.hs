import Prelude hiding (fail, pure, (>>=))

--------------------------------------------------------------------------------
-- Fallible

data Fallible e a = Success a | Failure e deriving Show

pure :: a -> Fallible e a
pure = Success

fail :: e -> Fallible e a
fail = Failure

(>>=) :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
fa >>= fb = case fa of
    Success a -> fb a
    Failure e -> fail e

catch :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
catch = error "Not implemented!"

dual :: Fallible e a -> Fallible a e
dual = \case
    Success a -> Failure a
    Failure e -> Success e

--------------------------------------------------------------------------------
-- Exercise

catch' :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
catch' = error "Not implemented!"

bind' :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
bind' = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

test0, test1, test2, test3, test4, test5, test6, test7 :: Fallible String String
test0 = catch' (pure "A") \_ -> pure "B"
test1 = catch' (pure "A") \_ -> fail "B"
test2 = catch' (fail "A") \_ -> pure "B"
test3 = catch' (fail "A") \_ -> fail "B"
test4 = bind' (pure "A") \_ -> pure "B"
test5 = bind' (pure "A") \_ -> fail "B"
test6 = bind' (fail "A") \_ -> pure "B"
test7 = bind' (fail "A") \_ -> fail "B"

main :: IO ()
main = mapM_ print [test0, test1, test2, test3, test4, test5, test6, test7]
