import Prelude hiding (pure, (>>=))

--------------------------------------------------------------------------------
-- State

data State s a = State (s -> (a, s))

pure :: a -> State s a
pure a = State \s -> (a, s)

runState :: State s a -> s -> (a, s)
runState (State f) = f

(>>=) :: State s a -> (a -> State s b) -> State s b
sa >>= sb = State \s ->
    case runState sa s of
        (a, s') -> runState (sb a) s'

--------------------------------------------------------------------------------
-- Exercise

get :: State s s
get = error "Not implemented!"

set :: s -> State s ()
set = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

test0, test1 :: State String String
test0 = get
test1 = set "B" >>= \() -> get

main :: IO ()
main = mapM_ (print . flip runState "A") [test0, test1]
