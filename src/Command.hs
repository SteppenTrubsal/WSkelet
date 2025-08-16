{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Command where

import           Data.Aeson
import           Data.Text    (Text)
import qualified Data.Text    as T

import           Core.Command
import Data.Data

data Command
  = C_GetPageCtx

type family Req c where
  Req 'C_GetPageCtx = ()

type family Res c where
  Res 'C_GetPageCtx = ()

data SCommand (c :: Command) where
  SGetPageCtx :: SCommand 'C_GetPageCtx

$(mkKnownCommandsInstances ''Command)

data SigmaCommand where
  SigmaCommand :: SCommand c -> SigmaCommand

parseCommandTag :: Text -> Maybe SigmaCommand
parseCommandTag t = case T.toLower t of
  "getpagectx" -> Just (SigmaCommand SGetPageCtx)
  _            -> Nothing

data InMsg where
  InMsg :: KnownCommand c => SCommand c -> Req c -> InMsg

data OutMsg where
  OutMsg :: KnownCommand c => SCommand c -> Res c -> OutMsg

instance ToJSON OutMsg where
  toJSON (OutMsg (sr :: SCommand c) payload) =
    object
      [ "tag"     .= tagOf sr
      , "payload" .= payload
      ]
    where
      tagOf :: KnownCommand c => SCommand c -> Text
      tagOf _ = commandTag (Proxy @c)

instance ToJSON InMsg where