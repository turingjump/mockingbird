module Mockingbird.Parse where

import           Data.Bifunctor        (first)
import           Data.Functor.Identity
import qualified Data.Text             as T
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Token

import Mockingbird.Types

twExpParser :: Parsec String u (TWExp String)
twExpParser = do
  e <- expParser
  orig <- optionMaybe $ reservedOp lang "|" >> lexeme lang (identifier lang)
  return $ TWExp
    { expression = e
    , originalPoster = orig
    }

expParser :: Parsec String u (Exp String)
expParser = do
  whiteSpace lang
  e <- many1 sub
  return $ foldl1 (:$) e
  where
    sub = parens lang expParser
     <|> (Var <$> lexeme lang (identifier lang))

lang :: GenTokenParser String u Identity
lang = makeTokenParser emptyDef
    { identStart = noneOf "()| \t\n"
    , identLetter = noneOf "()| \t\n"
    }

parseTweet :: T.Text -> Either T.Text (TWExp T.Text)
parseTweet s = fmap (fmap T.pack) $ first (T.pack . show)
             $ parse twExpParser "Parse Error" (T.unpack s)
