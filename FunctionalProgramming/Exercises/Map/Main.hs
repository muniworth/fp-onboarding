import Prelude hiding (pure, (>>=), map)

data State s a = State (s -> (a, s))

pure :: a -> State s a
pure a = State \s -> (a, s)

runState :: State s a -> s -> (a, s)
runState (State f) = f

(>>=) :: State s a -> (a -> State s b) -> State s b
sa >>= sb = State \s ->
    case runState sa s of
        (a, s') -> runState (sb a) s'

get :: State s s
get = error "Not implemented!"

set :: s -> State s ()
set = error "Not implemented!"

--------------------------------------------------------------------------------

data Q = Seen'' | Seen'x' | Seen'xy' | Seen'xyz'

transition :: Char -> State Q ()
transition nextChar = get >>= \currState ->
    case (currState, nextChar) of
        -- This is not the most economical set of cases.
        (Seen'', 'x') -> set Seen'x'
        (Seen'', _) -> set Seen''
        (Seen'x', 'x') -> set Seen'x'
        (Seen'x', 'y') -> set Seen'xy'
        (Seen'x', _) -> set Seen''
        (Seen'xy', 'x') -> set Seen'x'
        (Seen'xy', 'z') -> set Seen'xyz'
        (Seen'xy', _) -> set Seen''
        (Seen'xyz', _) -> set Seen'xyz'

statefulContains'xyz' :: String -> State Q Bool
statefulContains'xyz' = \case
    [] -> haveSeen'xyz'
    c:cs -> transition c >>= \() -> statefulContains'xyz' cs

contains'xyz' :: String -> Bool
contains'xyz' cs = fst (runState (statefulContains'xyz' cs) Seen'')

--------------------------------------------------------------------------------

map :: (a -> b) -> State s a -> State s b
map = error "Not implemented!"

haveSeen'xyz' :: State Q Bool
haveSeen'xyz' = error "Not implemented!"

--------------------------------------------------------------------------------

test0, test1, test2, test3, test4, test5 :: Bool
test0 = contains'xyz' "xyz"
test1 = contains'xyz' " xyz "
test2 = contains'xyz' "xxyz"
test3 = contains'xyz' "xyxyz"
test4 = contains'xyz' "x"
test5 = contains'xyz' "xy"

main :: IO ()
main = mapM_ print [test0, test1, test2, test3, test4, test5]
