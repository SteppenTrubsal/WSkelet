module Core(
  launch
) where

import           Network.Wai                    (Application)
import qualified Network.Wai.Handler.Warp       as WW
import qualified Network.Wai.Handler.WebSockets as WWS
import qualified Network.WebSockets             as WS

import           Auth (withUser)
import           Core.Env

app :: Env -> Application
app env = withUser env $ WWS.websocketsOr WS.defaultConnectionOptions

launch :: IO ()
launch = do
  env <- initEnv
  putStrLn "Running..."
  WW.run 8080 (app env)