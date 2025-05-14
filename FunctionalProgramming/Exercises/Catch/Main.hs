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

test0, test1, test2, test3 :: Fallible String Int
test0 = catch (pure 0) \_ -> pure 1
test1 = catch (pure 0) \_ -> fail "B"
test2 = catch (fail "A") \_ -> pure 1
test3 = catch (fail "A") \_ -> fail "B"

main :: IO ()
main = do
    print test0
    print test1
    print test2
    print test3
    return ()
