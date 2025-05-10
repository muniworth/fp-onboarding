import Prelude hiding (fail, pure)

data Fallible e a = Success a | Failure e deriving Show

pure :: a -> Fallible e a
pure = Success

fail :: e -> Fallible e a
fail = Failure

bind :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
bind fa fb = case fa of
    Success a -> fb a
    Failure e -> fail e

catch :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
catch = error "Not implemented!"

--test0, test1, test2 :: Fallible Int Int
test0 :: Fallible Int Int
test0 = catch (pure 0) \e -> pure 1

main :: IO ()
main = do
    print test0
    return ()
