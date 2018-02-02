{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
  -- the strings in this definition are Symbols, i.e. type-level Strinngs,
  -- enabled via DataKinds

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: UTCTime
}

type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
           :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI5 =
       "user" :> Capture "userid" Integer :> Get '[JSON] User
          -- equivalent to 'GET /user/:userid'
          -- except that we explicitly say "userid" :: Integer
  :<|> "user" :> Capture "userid" Integer :> DeleteNoContent '[JSON] NoContent
          -- equivalent to 'DELETE /user/:userid'

type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
          -- equivalent to 'GET /users?sortby={age, name}'
            -- TYPO ? shoudl that be a list of SortBys

type UserAPI7 = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
    -- equivalent to 'POST /users' with a JSON object
    -- describing a User in the request body. Returns a User encoded in JSON
  :<|> "users" :> Capture "userid" Integer :> ReqBody '[JSON] User
                                           :> Put '[JSON] User
    -- - equivalent to 'PUT /users/:userid' with a JSON object describing
    -- a User in the request body. Returns a User encoded in JSON

type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]

type ProtectedAPI11
     = UserAPI                              -- this is public
 :<|> BasicAuth "my-realm" User :> UserAPI2 -- this is protected by auth

type UserAPI12 innerAPI
     = UserAPI             -- this is the fixed bit of the API
 :<|> "inner" :> innerAPI  -- this lets us put various other APIs under /inner
