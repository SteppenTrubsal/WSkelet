{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}

module Core.Command where

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Language.Haskell.TH

class KnownCommand (c :: k) where
  commandTag :: proxy c -> Text

mkKnownCommandsInstances :: Name -> Q [Dec]
mkKnownCommandsInstances tyName = do
  info <- reify tyName
  case info of
    TyConI (DataD _ _ _ _ ctors _) -> do
      insts <- mapM mkInst ctors
      pure $ concat insts
    _ -> fail "mkKnownRouteInstances: expected data declaration"
  where
    mkInst :: Con -> Q [Dec]
    mkInst (NormalC conName []) = do
      fun      <- funD 'commandTag [clause [wildP] body []]
      instHead <- appT (conT ''KnownCommand) (pure promoted)
      pure [InstanceD Nothing [] instHead [fun]]
      where
        promoted = PromotedT conName
        tagTxt   = toTag $ nameBase conName
        tagExp   = litE (StringL tagTxt)
        body     = normalB [| T.pack $(tagExp) |]
    mkInst _ = fail "mkInst fail"

    toTag :: String -> String
    toTag s = map lower $ case s of
        'R' : '_' : rest -> rest 
        _ -> s 
      where
        lower c
          | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
          | otherwise            = c