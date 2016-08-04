module Mockingbird.Twitter where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource   (runResourceT)
import qualified Data.Conduit                   as C
import qualified Data.Conduit.List              as CL
import qualified Data.Text                      as T
import           Web.Twitter.Conduit            (Manager, call, stream, update,
                                                 userstream)
import           Web.Twitter.Conduit.Parameters (inReplyToStatusId)
import           Web.Twitter.Types

import Mockingbird.Combinators
import Mockingbird.Parse
import Mockingbird.Types

-- | Returns the reply the bird should make, if any
evalTweet :: Bird -> Status -> Maybe T.Text
evalTweet bird tweet = case parseTweet $ statusText tweet of
  Left _err -> Nothing
  Right e | nick bird == eHead e -> Just (pprint $ eval bird e)
          | otherwise            -> Nothing

-- | Handles one tweet, responding if appropriate
handleTweet :: Manager -> Bird -> Status -> IO ()
handleTweet mgr bird tweet = case evalTweet bird tweet of
  Nothing -> return ()
  Just response -> void $ call (account bird) mgr newTweet
    where
      newTweet = update response & inReplyToStatusId ?~ statusId tweet

-- | Get a twitter stream, and continously respond to new tweets when appropriate
processTweets :: Manager -> Bird -> IO ()
processTweets mgr bird = runResourceT $ do
  src <- stream (account bird) mgr userstream
  src C.$$+- CL.mapM_ (liftIO . go)
    where
      go (SStatus s) = handleTweet mgr bird s
      go _           = return ()
