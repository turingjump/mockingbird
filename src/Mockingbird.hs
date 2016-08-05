module Mockingbird
  ( Exp(..)
  , Bird(..)
  , Config(..)
  , parseTweet
  , evalTweet
  , pprint
  , eval
  -- * Birds
  , kBird
  , sBird
  , iBird
  , mBird
  , allBirds

  , testConfig
  , processAll

  , main
  ) where

import Mockingbird.Combinators
import Mockingbird.Parse
import Mockingbird.Twitter
import Mockingbird.Types

import Web.Twitter.Conduit (newManager, tlsManagerSettings)

main :: IO ()
main = do
  cfg <- twitterConfig <$> newManager tlsManagerSettings
  bs <- sequence [sBird, kBird, iBird, mBird]
  processAll (cfg { birds = bs })
