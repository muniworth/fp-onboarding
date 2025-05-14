import Prelude hiding (fail, pure, (>>=))

data Fallible e a = Success a | Failure e deriving Show

pure :: a -> Fallible e a
pure = Success

fail :: e -> Fallible e a
fail = Failure

(>>=) :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
(>>=) fa fb = case fa of
    Success a -> fb a
    Failure e -> fail e

--------------------------------------------------------------------------------

catch :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
catch = error "Not implemented!"

--------------------------------------------------------------------------------

test0, test1, test2, test3 :: Fallible String String
test0 = catch (pure "A") \_ -> pure "B"
test1 = catch (pure "A") \_ -> fail "B"
test2 = catch (fail "A") \_ -> pure "B"
test3 = catch (fail "A") \_ -> fail "B"

main :: IO ()
main = mapM_ print [test0, test1, test2, test3]
