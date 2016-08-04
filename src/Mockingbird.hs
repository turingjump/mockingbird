module Mockingbird
  ( Exp(..)
  , TwitterAccount(..)
  , Bird(..)
  , parseTweet
  , pprint
  , eval
  -- * Birds
  , kBird
  , sBird
  , iBird

  , main
  ) where

import Mockingbird.Combinators
import Mockingbird.Parse
import Mockingbird.Twitter
import Mockingbird.Types

import Control.Concurrent  (forkIO)
import Control.Monad
import Web.Twitter.Conduit (newManager, tlsManagerSettings)

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  void $ forkIO . processTweets mgr <$> sBird
  void $ forkIO . processTweets mgr <$> kBird
  void $ forkIO . processTweets mgr <$> iBird
