import Control.Applicative
import Data.Char
import Data.Functor

data ParseError = UnexpectedChar Char | UnexpectedEof deriving Show
data ParseResult a = Ok a String | Error [ParseError] deriving Show
newtype Parse a = Parse (String -> ParseResult a)

instance Functor Parse where
    fmap = liftA

instance Applicative Parse where
    pure a = Parse (Ok a)

    Parse pf <*> Parse pa = Parse \s ->
        case pf s of
            Ok f s' -> case pa s' of
                Ok a s'' -> Ok (f a) s''
                Error e -> Error e
            Error e -> Error e

instance Alternative Parse where
    empty = Parse \_ -> Error []

    (Parse px) <|> (Parse py) = Parse \s ->
        case px s of
            Ok x s' -> Ok x s'
            Error e -> case py s of
                Ok y s' -> Ok y s'
                Error e' -> Error (e <> e')

satisfy :: (Char -> Bool) -> Parse Char
satisfy pred = Parse \case
    x:xs -> if pred x then Ok x xs else Error [UnexpectedChar x]
    [] -> Error [UnexpectedEof]

eof :: Parse ()
eof = Parse \case
    x:_ -> Error [UnexpectedChar x]
    [] -> Ok () []

runParse :: Parse a -> String -> ParseResult a
runParse (Parse f) = f

--------------------------------------------------------------------------------

whitespace :: Parse String
whitespace = many $ satisfy isSpace

token :: Parse a -> Parse a
token p = p <* whitespace

alphaNum :: Parse String
alphaNum = token . some $ satisfy isAlphaNum

symbol :: Char -> Parse ()
symbol c = token . void $ satisfy (== c)

data Term
    = Atom String
    | Add Term Term
    | Mul Term Term
    deriving Show

--parseTerm :: Parse Term
--parseTerm = (Atom <$> parseAtomToken)
--    <|> (parseSymbolToken '+' *> (Add <$> parseTerm <*> parseTerm))
--    <|> (parseSymbolToken '*' *> (Mul <$> parseTerm <*> parseTerm))

parseTerm0, parseTerm1, parseTerm2 :: Parse Term
parseTerm2 = Atom <$> alphaNum
         <|> symbol '(' *> parseTerm0 <* symbol ')'
parseTerm1 = Mul <$> parseTerm2 <* symbol '*' <*> parseTerm1
         <|> parseTerm2
parseTerm0 = Add <$> parseTerm1 <* symbol '+' <*> parseTerm0
         <|> parseTerm1

parseTerm :: Parse Term
parseTerm = whitespace *> parseTerm0 <* eof

main :: IO ()
main = print $ runParse parseTerm "a * (b + c)"
