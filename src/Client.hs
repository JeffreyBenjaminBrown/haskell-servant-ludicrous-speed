{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Show, Generic)
instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)
instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)
instance FromJSON Email

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
api :: Proxy API
api = Proxy

position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> ClientM Position
hello :: Maybe String -- ^ an optional value for "name"
      -> ClientM HelloMessage
marketing :: ClientInfo -- ^ value for the request body
          -> ClientM Email
position :<|> hello :<|> marketing = client api -- ^ magic, not a typo

queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, message, em)

-- | Run Server.main3 before running this, so it has something to talk to.
run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em


-- | Sidenote: if there's an empty endpoint in the API, the client must know:
type API' = API :<|> EmptyAPI

api' :: Proxy API'
api' = Proxy

(position' :<|> hello' :<|> marketing') :<|> EmptyClient = client api'
