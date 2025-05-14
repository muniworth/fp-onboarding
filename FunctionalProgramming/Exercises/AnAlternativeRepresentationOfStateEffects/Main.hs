{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (intercalate)
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

get :: State s s
get = error "Not implemented!"

set :: s -> State s ()
set = error "Not implemented!"

--------------------------------------------------------------------------------
-- State'

data State' s a
    = Pure a
    | Get (s -> State' s a)
    | Set s (State' s a)

--------------------------------------------------------------------------------
-- Exercise, Part 1

pure' :: a -> State' s a
pure' = error "Not implemented!"

bind' :: State' s a -> (a -> State' s b) -> State' s b
bind' = error "Not implemented!"

get' :: State' s s
get' = error "Not implemented!"

set' :: s -> State' s ()
set' = error "Not implemented!"

--------------------------------------------------------------------------------
-- Exercise, Part 2

interpretA :: State s a -> State' s a
interpretA = error "Not implemented!"

interpretB :: State' s a -> State s a
interpretB = error "Not implemented!"

--------------------------------------------------------------------------------
-- Tests

class Enumerable a where
    enumerate :: [a]

instance Enumerable Bool where
    enumerate = [False, True]

instance (Enumerable a, Show a, Show b) => Show (a -> b) where
    show f =
        let show1 a = "\\" ++ show a ++ " -> " ++ show (f a)
        in "[" ++ intercalate ", " (show1 <$> enumerate) ++ "]"

instance (Enumerable s, Show s, Show a) => Show (State s a) where
    show = show . runState

instance (Enumerable s, Show s, Show a) => Show (State' s a) where
    show = \case
        Pure a -> "Pure (" ++ show a ++ ")"
        Get k -> "Get (" ++ show k ++ ")"
        Set s k -> "Set (" ++ show s ++ ") (" ++ show k ++ ")"

main :: IO ()
main = do
    print . interpretA $ (pure "A" :: State Bool String)
    print . interpretA $ (get :: State Bool Bool)
    print . interpretA $ set True
    print . interpretB $ (pure' "A" :: State' Bool String)
    print . interpretB $ (get' :: State' Bool Bool)
    print . interpretB $ set' True
