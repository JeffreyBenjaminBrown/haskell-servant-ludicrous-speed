{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html


-- | == Common
data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User


-- | == One endpoint
users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

type UserAPI1 = "users" :> Get '[JSON] [User]

server1 :: Server UserAPI1
server1 = return users1

main :: IO ()
main = run 8081 $ serve (Proxy :: Proxy UserAPI1) server1
-- Once that's running, this is a qay to query it:
-- curl http://localhost:8081/users


-- | == Multiple endpoints
type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
users2 :: [User]
users2 = [isaac, albert]

-- | order here must match that in UserAPI2
server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac

main2 :: IO ()
main2 = run 8081 $ serve (Proxy :: Proxy UserAPI2) server2
-- curl http://localhost:8081/users


-- | == QueryParam, Capture and ReqBody
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic
instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic
instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic
instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic
instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"

server3 :: Server API
server3 = position :<|> hello :<|> marketing
  where position :: Int -> Int -> Handler Position
        position x y = return $ Position x y

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientinfo = return (emailForClient clientinfo)

main3 :: IO ()
main3 = run 8081 $ serve (Proxy :: Proxy API) server3
-- curl http://localhost:8081/position/1/2
-- curl http://localhost:8081/hello
-- curl http://localhost:8081/hello?name=Alp
-- curl -d '{"clientName":"Alp Mestanogullari", "clientEmail" : "alp@foo.com", "clientAge": 25, "clientInterestedIn": ["haskell", "mathematics"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing


-- | == the FromHttpApiData and ToHttpApiData classes

data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS

-- For this tutorial to compile 'HTMLLucid' and 'HTMLBlaze' have to be
-- distinct. Usually you would stick to one html rendering library and then
-- you can go with one 'HTML' type.
data HTMLBlaze

instance Accept HTMLBlaze where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender HTMLBlaze a where
    mimeRender _ = renderHtml . Text.Blaze.Html.toHtml

instance MimeRender HTMLBlaze Text.Blaze.Html.Html where
    mimeRender _ = renderHtml

type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]

data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic -- for the JSON instance

instance ToJSON Person

-- HTML serialization of a single person
instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons

  toHtmlRaw = toHtml

people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

main4 :: IO ()
main4 = run 8081 $ serve (Proxy :: Proxy PersonAPI) $ return people
-- curl http://localhost:8081/persons
-- curl -H 'Accept: text/html' http://localhost:8081/persons


-- | ==== The Handler monad

-- | == IO
type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent { content :: String } deriving Generic
instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do filecontent <- liftIO (readFile "myfile.txt")
             return (FileContent filecontent)

main5 :: IO ()
main5 = run 8081 $ serve (Proxy :: Proxy IOAPI1) server5


-- | == the FileContent wrapper is unnecessary (but seems helpful)
type IOAPI1' = "myfile.txt" :> Get '[JSON] String

server5' :: Server IOAPI1'
server5' = do filecontent <- liftIO (readFile "myfile.txt")
              return filecontent

main5' :: IO ()
main5' = run 8081 $ serve (Proxy :: Proxy IOAPI1') server5'


-- | == Error (and IO)
server6 :: Server IOAPI1
server6 = do exists <- liftIO (doesFileExist "myfile.txt")
             if exists
               then liftIO (readFile "myfile.txt") >>= return . FileContent
               else throwError custom404Err
  where custom404Err = err404 { errBody = "myfile.txt not found." }

main6 = run 8081 $ serve (Proxy :: Proxy IOAPI1) server6

-- try this both when myfile.txt exists and when not
-- curl --verbose http://localhost:8081/myfile.txt


-- | == Response headers
type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myHandler :: Server MyHandler
myHandler = return $ addHeader 1797 albert

type MyHeadfulHandler = Get '[JSON] (Headers '[Header "X-A-Bool" Bool, Header "X-An-Int" Int] User)

myHeadfulHandler :: Server MyHeadfulHandler
myHeadfulHandler = return $ addHeader True $ addHeader 1797 albert


-- | = sometimes, "you have to explicitly not add a header by using noHeader"
type MyMaybeHeaderHandler
  = Capture "withHeader" Bool :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myMaybeHeaderHandler :: Server MyMaybeHeaderHandler
myMaybeHeaderHandler x = return $ if x then addHeader 1797 albert
                                       else noHeader albert


-- | ==== Serving static files
type StaticAPI = "static" :> Raw

main7 = run 8081 $ serve (Proxy :: Proxy StaticAPI) $ serveDirectoryWebApp "/home/jeff"
--curl http://localhost:8081/static/some-file-in-my-home-folder


-- | For running a server, that's as far as I read.
-- The client section, however, is complete (and much shorter).


-- | ==== Nested APIs

--type UserAPI3 = -- view the user with given userid, in JSON
--                Capture "userid" Int :> Get '[JSON] User
--
--           :<|> -- delete the user with given userid. empty response
--                Capture "userid" Int :> DeleteNoContent '[JSON] NoContent
--
--type UserAPI4 = Capture "userid" Int :>
--  (    Get '[JSON] User
--  :<|> DeleteNoContent '[JSON] NoContent
--  )
--
--Server UserAPI3 = (Int -> Handler User)
--             :<|> (Int -> Handler NoContent)
--
--Server UserAPI4 = Int -> (    Handler User
--                         :<|> Handler NoContent
--                         )
--
--server8 :: Server UserAPI3
--server8 = getUser :<|> deleteUser
--
--  where getUser :: Int -> Handler User
--        getUser _userid = error "..."
--
--        deleteUser :: Int -> Handler NoContent
--        deleteUser _userid = error "..."
--
---- notice how getUser and deleteUser
---- have a different type! no argument anymore,
---- the argument directly goes to the whole Server
--server9 :: Server UserAPI4
--server9 userid = getUser userid :<|> deleteUser userid
--
--  where getUser :: Int -> Handler User
--        getUser = error "..."
--
--        deleteUser :: Int -> Handler NoContent
--        deleteUser = error "..."
--
---- we just factor out the "users" path fragment
--type API1 = "users" :>
--  (    Get '[JSON] [User] -- user listing
--  :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular user
--  )
--
---- we factor out the Request Body
--type API2 = ReqBody '[JSON] User :>
--  (    Get '[JSON] User -- just display the same user back, don't register it
--  :<|> PostNoContent '[JSON] NoContent  -- register the user. empty response
--  )
--
---- we factor out a Header
--type API3 = Header "Authorization" Token :>
--  (    Get '[JSON] SecretData -- get some secret data, if authorized
--  :<|> ReqBody '[JSON] SecretData :> PostNoContent '[JSON] NoContent -- add some secret data, if authorized
--  )
--
--newtype Token = Token ByteString
--newtype SecretData = SecretData ByteString
--
--type UsersAPI =
--       Get '[JSON] [User] -- list users
--  :<|> ReqBody '[JSON] User :> PostNoContent '[JSON] NoContent -- add a user
--  :<|> Capture "userid" Int :>
--         ( Get '[JSON] User -- view a user
--      :<|> ReqBody '[JSON] User :> PutNoContent '[JSON] NoContent -- update a user
--      :<|> DeleteNoContent '[JSON] NoContent -- delete a user
--         )
--
--usersServer :: Server UsersAPI
--usersServer = getUsers :<|> newUser :<|> userOperations
--
--  where getUsers :: Handler [User]
--        getUsers = error "..."
--
--        newUser :: User -> Handler NoContent
--        newUser = error "..."
--
--        userOperations userid =
--          viewUser userid :<|> updateUser userid :<|> deleteUser userid
--
--          where
--            viewUser :: Int -> Handler User
--            viewUser = error "..."
--
--            updateUser :: Int -> User -> Handler NoContent
--            updateUser = error "..."
--
--            deleteUser :: Int -> Handler NoContent
--            deleteUser = error "..."
--type ProductsAPI =
--       Get '[JSON] [Product] -- list products
--  :<|> ReqBody '[JSON] Product :> PostNoContent '[JSON] NoContent -- add a product
--  :<|> Capture "productid" Int :>
--         ( Get '[JSON] Product -- view a product
--      :<|> ReqBody '[JSON] Product :> PutNoContent '[JSON] NoContent -- update a product
--      :<|> DeleteNoContent '[JSON] NoContent -- delete a product
--         )
--
--data Product = Product { productId :: Int }
--
--productsServer :: Server ProductsAPI
--productsServer = getProducts :<|> newProduct :<|> productOperations
--
--  where getProducts :: Handler [Product]
--        getProducts = error "..."
--
--        newProduct :: Product -> Handler NoContent
--        newProduct = error "..."
--
--        productOperations productid =
--          viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid
--
--          where
--            viewProduct :: Int -> Handler Product
--            viewProduct = error "..."
--
--            updateProduct :: Int -> Product -> Handler NoContent
--            updateProduct = error "..."
--
--            deleteProduct :: Int -> Handler NoContent
--            deleteProduct = error "..."
--type CombinedAPI = "users" :> UsersAPI
--              :<|> "products" :> ProductsAPI
--
--server10 :: Server CombinedAPI
--server10 = usersServer :<|> productsServer
--
---- API for values of type 'a'
---- indexed by values of type 'i'
--type APIFor a i =
--       Get '[JSON] [a] -- list 'a's
--  :<|> ReqBody '[JSON] a :> PostNoContent '[JSON] NoContent -- add an 'a'
--  :<|> Capture "id" i :>
--         ( Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
--      :<|> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent -- update an 'a'
--      :<|> DeleteNoContent '[JSON] NoContent -- delete an 'a'
--         )
--
---- Build the appropriate 'Server'
---- given the handlers of the right type.
--serverFor :: Handler [a] -- handler for listing of 'a's
--          -> (a -> Handler NoContent) -- handler for adding an 'a'
--          -> (i -> Handler a) -- handler for viewing an 'a' given its identifier of type 'i'
--          -> (i -> a -> Handler NoContent) -- updating an 'a' with given id
--          -> (i -> Handler NoContent) -- deleting an 'a' given its id
--          -> Server (APIFor a i)
--serverFor = error "..."
---- implementation left as an exercise. contact us on IRC
---- or the mailing list if you get stuck!
--
--type CombinedAPI2 = API :<|> "empty" :> EmptyAPI
--
--server11 :: Server CombinedAPI2
--server11 = server3 :<|> emptyServer


-- | ==== Using another monad for handlers
--type Server api = ServerT api Handler


-- | == Natural transformations

--newtype m :~> n = NT { ($$) :: forall a. m a -> n a}
--
--listToMaybeNT :: [] :~> Maybe
--listToMaybeNT = NT listToMaybe  -- from Data.Maybe
--
--readerToHandler :: Reader String :~> Handler
--
--readerToHandler' :: forall a. Reader String a -> Handler a
--readerToHandler' r = return (runReader r "hi")
--
--readerToHandler :: Reader String :~> Handler
--readerToHandler = NT readerToHandler'
--
--type ReaderAPI = "a" :> Get '[JSON] Int
--            :<|> "b" :> Get '[JSON] String
--
--readerAPI :: Proxy ReaderAPI
--readerAPI = Proxy
--
--readerServerT :: ServerT ReaderAPI (Reader String)
--readerServerT = a :<|> b
--
--  where a :: Reader String Int
--        a = return 1797
--
--        b :: Reader String String
--        b = ask
--
--readerServer :: Server ReaderAPI
--readerServer = enter readerToHandler readerServerT
--
--app4 :: Application
--app4 = serve readerAPI readerServer
--
---- curl http://localhost:8081/a
---- curl http://localhost:8081/b
--
--
