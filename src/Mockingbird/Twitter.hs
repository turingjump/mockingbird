module Mockingbird.Twitter where

import           Control.Concurrent
import           Control.Lens                   ((&), (?~))
import           Control.Monad
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource   (runResourceT)
import qualified Data.Conduit                   as C
import qualified Data.Conduit.List              as CL
import           Data.Monoid
import qualified Data.Text                      as T
import qualified System.Log.Logger              as Log
import           Web.Twitter.Conduit            (Manager, callWithResponse,
                                                 responseStatus, stream, update,
                                                 userstream)
import           Web.Twitter.Conduit.Parameters (inReplyToStatusId)
import           Web.Twitter.Types

import Network.HTTP.Types.Status (statusCode)

import Mockingbird.Combinators
import Mockingbird.Parse
import Mockingbird.Types

twitterConfig :: Manager -> Config
twitterConfig mgr = Config
  { tweetStream = \bird -> stream (account bird) mgr userstream
  , postTweet = handleTweet mgr
  , birds = []
  }

testConfig :: [Status] -> (Bird -> Status -> IO ()) -> Config
testConfig statuses post = Config
  { tweetStream = const . return . C.newResumableSource
                $ mapM_ (C.yield . SStatus) statuses
  , postTweet = post
  , birds = []
  }

-- | Returns the reply the bird should make.
evalTweet :: Bird -> Status -> Maybe T.Text
evalTweet bird tweet = case parseTweet $ statusText tweet of
  Left err -> Just err
  Right e | eval bird e /= e     -> case eval bird e of
              t                     -> Just $ pprint t
                { originalPoster = Just $ userScreenName (statusUser tweet)}
          | otherwise            -> Nothing

-- | Handles one tweet, responding if appropriate
handleTweet :: Manager -> Bird -> Status -> IO ()
handleTweet mgr bird tweet = case evalTweet bird tweet of
  Nothing -> return ()
  Just response -> do
    Log.infoM "Mockingbird" $ "Posting: " <> show newTweet
    resp <- callWithResponse (account bird) mgr newTweet
    when (statusCode (responseStatus resp) > 300) $
      Log.errorM "Mockinbird" $ "Error posting: " <> show resp

    where
      newTweet = update response & inReplyToStatusId ?~ statusId tweet

-- | Get a twitter stream, and continously respond to new tweets when appropriate
processTweets :: Config -> Bird -> IO ()
processTweets cfg bird = runResourceT $ do
  src <- tweetStream cfg bird
  src C.$$+- CL.mapM_ (liftIO . go)
    where
      go (SStatus s) = postTweet cfg bird s
      go _           = return ()

-- | Process tweets for all birds
processAll :: Config -> IO ()
processAll cfg = do
  children <- newMVar []
  mapM_ (forkChild children . processTweets cfg) (birds cfg)
  waitForChildren children
  where

    forkChild :: MVar [MVar ()] -> IO () -> IO ThreadId
    forkChild children action = do
      mvar <- newEmptyMVar
      modifyMVar_ children (\x -> return $ mvar:x)
      forkFinally action (const $ putMVar mvar ())

    waitForChildren ::  MVar [MVar ()] -> IO ()
    waitForChildren children = do
      cs <- takeMVar children
      case cs of
        []   -> return ()
        m:ms -> do
           putMVar children ms
           takeMVar m
           waitForChildren children
