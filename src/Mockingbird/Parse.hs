module Mockingbird.Parse where

import           Data.Bifunctor       (first)
import qualified Data.Text            as T
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Token

import Mockingbird.Types

parser :: Parsec String u (Exp String)
parser = do
  whiteSpace lang
  e <- many1 sub
  return $ foldl1 (:$) e
  where
    lang = makeTokenParser emptyDef{
       identStart = noneOf "() \t\n"
     , identLetter = noneOf "() \t\n"
    }
    sub = parens lang parser
     <|> (Var <$> lexeme lang (identifier lang))

parseTweet :: T.Text -> Either T.Text (Exp T.Text)
parseTweet s
  = fmap (fmap T.pack) $ first (T.pack . show) $ parse parser "(tweet)" (T.unpack s)
