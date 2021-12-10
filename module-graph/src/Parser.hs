module Parser where

import           Control.Applicative
import           Control.Monad

import           Control.Arrow (first, second)

newtype Parser a = MkParser { runParser :: String -> Maybe (String, a) }

parse :: Parser a -> String -> (String, a)
parse p str =
  case runParser (p <* many parseNewline) str of
    Nothing -> error "parse: Parse error"
    Just r -> r

instance Functor Parser where
  fmap f (MkParser p) = MkParser (fmap (fmap f) . p)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = MkParser (\s -> (Just (s, x)))

  MkParser p >>= f =
    MkParser $ \str -> do
      (str', x) <- p str
      runParser (f x) str'

instance Alternative Parser where
  empty = MkParser (const Nothing)
  MkParser p <|> MkParser q = MkParser (\str -> p str <|> q str)

parseWhen :: (Char -> Bool) -> Parser Char
parseWhen p = MkParser go
  where
    go [] = Nothing
    go (c:cs) =
      if p c
        then Just (cs, c)
        else Nothing

parseChar :: Char -> Parser Char
parseChar c = parseWhen (== c)

parseString :: String -> Parser String
parseString str = mapM parseChar str

parseNotString :: String -> Parser String
parseNotString str = MkParser go
  where
    lenStr = length str

    go [] = Nothing
    go str'@(c:cs)
      | take lenStr str' == str = Just (str', [])
      | otherwise = second (c:) <$> go cs

parseAs :: String -> a -> Parser a
parseAs str x = parseString str *> pure x

parseDigit :: Parser Char
parseDigit = parseWhen (`elem` ['0'..'9'])

parseInt :: Parser Int
parseInt = fmap read (some parseDigit)

parseSpace :: Parser Char
parseSpace = parseWhen (`elem` " \t\n")

parseNewline :: Parser Char
parseNewline = parseChar '\n'

parseAlphanum :: Parser Char
parseAlphanum = parseWhen (`elem` (['a'..'z'] ++ ['A'..'Z'])) <|> parseDigit

