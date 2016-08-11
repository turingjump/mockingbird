module Mockingbird.Combinators where

import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import qualified Data.Text             as T
import           Mockingbird.Types
import           Web.Twitter.Conduit   (Credential (..), OAuth (..), def,
                                        setCredential, twitterOAuth)

import Debug.Trace

eval :: Bird -> TWExp T.Text -> TWExp T.Text
eval b te = te { expression = getCombinator (definition b) (expression te) }

allBirds :: IO [Bird]
allBirds = sequence [sBird, kBird, iBird, mBird]

-- TODO: This way of defining birds is error-prone and repetitive. Alo
-- | @K@ or constant bird
-- Definition: @ K x y = x @
kBird :: IO Bird
kBird = makeBird "secret/kbird.txt" go
  where
    go name (r@(_ :$ _ :$ _) :$ d) = go name r :$ d
    go name r@(n :$ y :$ z)
      | n == Var ("@" <> name) = y
      | otherwise              = case eHead (n :$ y) of
          Nothing -> n :$ y :$ go name z
          Just h -> if h == name then go name (n :$ y) :$ z else r
    go name r@(a :$ b) = case eHead a of
      Nothing -> a :$ traceShowId (go name b)
      Just h -> if h == name then go name a :$ b else r
    go _ z = z

-- | @S@ bird
-- Definition: @ S x y z = x z (y z)
sBird :: IO Bird
sBird = makeBird "secret/sbird.txt" go
  where
    go name (r@(_ :$ _ :$ _ :$ _) :$ d) = go name r :$ d
    go name r@(n :$ x :$ y :$ z)
      | n == Var ("@" <> name) = x :$ z :$ (y :$ z)
      | otherwise              = case eHead (n :$ x :$ y) of
          Nothing -> n :$ x:$ y :$ go name z
          Just h -> if h == name then go name (n :$ x :$ y) :$ z else r
    go name r@(a :$ b) = case eHead a of
      Nothing -> a :$ go name b
      Just h -> if h == name then go name a :$ b else r
    go _ z = z

-- | @I@ or identity bird
-- Definition: @ I x = x @
iBird :: IO Bird
iBird = makeBird "secret/ibird.txt" go
  where
    go name (r@(_ :$ _) :$ d) = go name r :$ d
    go name r@(n :$ x) | n == Var ("@" <> name) = x
                       | otherwise              = case eHead n of
      Nothing -> n :$ go name x
      Just h -> if h == name then go name n :$ x else r
    go _ z = z

-- | @M@ bird.
-- Definition: @ M x = x x @
mBird :: IO Bird
mBird = makeBird "secret/mbird.txt" go
  where
    go name (r@(_ :$ _) :$ d) = go name r :$ d
    go name r@(n :$ x) | n == Var ("@" <> name) = x :$ x
                       | otherwise              = case eHead n of
      Nothing -> n :$ go name x
      Just h -> if h == name then go name n :$ x else r
    go _ z = z

makeBird :: FilePath -> (T.Text -> Exp T.Text -> Exp T.Text) -> IO Bird
makeBird file comb = do
  contents <- readFile file
  case lines contents of
    [name, consumerKey, consumerSecret, apiToken, apiTokenSecret]
       -> return Bird { nick = T.pack name
                      , account = twInfo
                      , definition = Combinator $ comb $ T.pack name
                      }
          where
            twInfo = setCredential oauth credential def
            oauth = twitterOAuth { oauthConsumerKey = BS.pack consumerKey
                                 , oauthConsumerSecret = BS.pack consumerSecret
                                 }
            credential = Credential [ ("oauth_token", BS.pack apiToken)
                                    , ("oauth_token_secret", BS.pack apiTokenSecret)
                                    ]
    _  -> error "expecting exactly five lines"
