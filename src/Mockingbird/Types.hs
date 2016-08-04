module Mockingbird.Types where

import           Data.Monoid
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           Web.Twitter.Conduit.Types (TWInfo)

data Exp x
  = Exp x :$ Exp x
  | Var x
  deriving (Eq, Show, Read, Generic, Functor)

infixl 2 :$

eHead :: Exp x -> x
eHead (Var x) = x
eHead (i :$ _) = eHead i

pprint :: Exp T.Text -> T.Text
pprint (Var x) = x
pprint (a :$ Var b) = pprint a <> " " <> b
pprint (a :$ b) = pprint a <> " (" <> pprint b <> ")"

data TwitterAccount = TwitterAccount
  { accountName :: String
  } deriving (Eq, Show, Read, Generic)

newtype Combinator = Combinator { getCombinator :: Exp T.Text -> Exp T.Text }
  deriving (Generic)

data Bird = Bird
  { nick       :: T.Text
  , account    :: TWInfo
  , definition :: Combinator
  } deriving (Generic)
