module Mockingbird
  ( Exp(..)
  , TWExp(..)
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
import qualified System.Log.Handler.Syslog as Log
import qualified System.Log as Log
import qualified System.Log.Logger as Log

main :: IO ()
main = do
  slog <- Log.openlog "Mockingbird" [Log.PID, Log.PERROR] Log.USER Log.DEBUG
  Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler slog)
  cfg <- twitterConfig <$> newManager tlsManagerSettings
  bs <- sequence [sBird, kBird, iBird, mBird]
  processAll (cfg { birds = bs })
