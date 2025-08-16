{-# LANGUAGE DeriveGeneric #-}
module Core.Env where

import           Control.Concurrent.STM
import qualified Data.Vault.Lazy        as V
import           Data.Text (Text)
import           GHC.Generics


newtype UserId = UserId { unUserId :: Text } deriving ()

data User = User
  { userId :: UserId
  } deriving (Generic)

data Env = Env
  { envUserKey   :: V.Key User
  , envBroadcast :: TChan Text
  }

initEnv :: IO Env
initEnv =
  Env <$> V.newKey <*> newTChanIO