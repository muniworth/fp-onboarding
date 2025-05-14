import Prelude hiding (fail, pure, (>>=), read)

--------------------------------------------------------------------------------
-- Fallible

data Fallible e a = Success a | Failure e deriving Show

fpure :: a -> Fallible e a
fpure = Success

fail :: e -> Fallible e a
fail = Failure

fbind :: Fallible e a -> (a -> Fallible e b) -> Fallible e b
fa `fbind` fb = case fa of
    Success a -> fb a
    Failure e -> fail e

catch :: Fallible e a -> (e -> Fallible f a) -> Fallible f a
catch = error "Not implemented!"

--------------------------------------------------------------------------------
-- State

data State s a = State (s -> (a, s))

spure :: a -> State s a
spure a = State \s -> (a, s)

runState :: State s a -> s -> (a, s)
runState (State f) = f

sbind :: State s a -> (a -> State s b) -> State s b
sa `sbind` sb = State \s ->
    case runState sa s of
        (a, s') -> runState (sb a) s'

get :: State s s
get = error "Not implemented!"

set :: s -> State s ()
set = error "Not implemented!"

--------------------------------------------------------------------------------
-- File IO

type FileName = String
type FileContents = String

data FileIO a
  = Pure a
  | Exists FileName (Bool -> FileIO a)
  | Read FileName (FileContents -> FileIO a)
  | Write FileName FileContents (FileIO a)
  | Delete FileName (FileIO a)

pure :: a -> FileIO a
pure = Pure

exists :: FileName -> FileIO Bool
exists x = Exists x pure

read :: FileName -> FileIO FileContents
read x = Read x pure

write :: FileName -> FileContents -> FileIO ()
write x s = Write x s (pure ())

delete :: FileName -> FileIO ()
delete x = Delete x (pure ())

(>>=) :: FileIO a -> (a -> FileIO b) -> FileIO b
fa >>= fb = case fa of
    Pure a -> fb a
    Exists x k -> Exists x \e -> k e >>= fb
    Read x k -> Read x \s -> k s >>= fb
    Write x s k -> Write x s (k >>= fb)
    Delete x k -> Delete x (k >>= fb)

--------------------------------------------------------------------------------
-- Exercise

data FileError

data FileSystem

simulate :: FileIO a -> State FileSystem (Fallible FileError a)
simulate = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

-- You'll need to write your own tests (if you want).

main :: IO ()
main = return ()
