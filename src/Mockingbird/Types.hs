module Mockingbird.Types where

import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (ResumableSource)
import           Data.Monoid
import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Web.Twitter.Conduit.Types    (TWInfo)
import           Web.Twitter.Types            (Status, StreamingAPI)

data Exp x
  = Exp x :$ Exp x
  | Var x
  deriving (Eq, Show, Read, Generic, Functor)

infixl 2 :$

data TWExp x = TWExp
  { expression     :: Exp x
  , originalPoster :: Maybe x
  } deriving (Eq, Show, Read, Generic, Functor)


eHead :: TWExp x -> x
eHead e = case expression e of
  Var x  -> x
  i :$ _ -> eHead $ TWExp i Nothing

pprint :: TWExp T.Text -> T.Text
pprint tw = case originalPoster tw of
  Nothing -> pprintExp (expression tw)
  Just op -> pprintExp (expression tw) <> " | " <> op
  where
    pprintExp e = case e of
      Var x -> x
      a :$ Var b -> pprintExp a <> " " <> b
      a :$ b -> pprintExp a <> " (" <> pprintExp b <> ")"

data Config = Config
  { tweetStream :: Bird -> ResourceT IO (ResumableSource (ResourceT IO) StreamingAPI)
  , postTweet   :: Bird -> Status -> IO ()
  , birds       :: [Bird]
  } deriving (Generic)

newtype Combinator = Combinator { getCombinator :: Exp T.Text -> Exp T.Text }
  deriving (Generic)

data Bird = Bird
  { nick       :: T.Text
  , account    :: TWInfo
  , definition :: Combinator
  } deriving (Generic)
