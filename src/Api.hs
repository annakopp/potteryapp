{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Reader         (ReaderT, runReaderT, lift, liftM)
import Control.Monad.IO.Class       (liftIO)
import Network.Wai                  (Application)
import Control.Error
import Database.Persist.Postgresql  (selectList, Entity(..), (==.)
                                    , fromSqlKey, insert)
import Data.Int                     (Int64)
import Servant

import Config    (Config(..))

import Models
import Data.Text hiding (map)
import Data.Text.Encoding
import Crypto.PasswordStore

type PotteryAPI =
         "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "users" :> "sign_in" :> ReqBody '[JSON] SignInUser :> Post '[JSON] User
    :<|> "users" :> Capture "id" UserId :> "projects" :> Get '[JSON] [PotteryProject]
    :<|> "project"  :> ReqBody '[JSON] PotteryProject :> Post '[JSON] Int64
    :<|> "project"  :> Capture "id" PotteryProjectId :> Get '[JSON] PotteryProject

type AppM = ReaderT Config (ExceptT ServantErr IO)

potteryAPI :: Proxy PotteryAPI
potteryAPI = Proxy

readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server PotteryAPI
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve potteryAPI (readerServer cfg)

server :: ServerT PotteryAPI AppM
server = postUser :<|> signInUser :<|> getProjects :<|> postProject :<|> getProject

postUser :: User -> AppM Int64
postUser user = do
    pwdHash <- liftIO $ decodeUtf8 <$> makePassword (encodeUtf8 . userPassword $ user) 17
    liftM fromSqlKey . runDb . insert $ user{userPassword = pwdHash}

signInUser :: SignInUser -> AppM User
signInUser u = do
    dbUser <- runDb $ selectList [UserEmail ==. signInEmail u] []
    let userList = map (\(Entity _ y) -> y) dbUser
    case userList of
        [] -> lift $ throwE err401
        (x:xs) -> if verifyPassword (encodeUtf8 . signInPassword $ u) (encodeUtf8 . userPassword $ x)
                  then return x
                  else lift $ throwE err401


getProjects :: UserId -> AppM [PotteryProject]
getProjects uid = do
    projs <- runDb $ selectList [PotteryProjectUser ==. uid] []
    let projList = map (\(Entity _ y) -> y) projs
    return projList

getProject :: PotteryProjectId -> AppM PotteryProject
getProject projectId = do
    projects <- runDb $ selectList [PotteryProjectId ==. projectId] []
    let list = map (\(Entity _ y) -> y) projects
    case list of
        [] -> lift $ throwE err404
        (x:xs) -> return x

postProject :: PotteryProject -> AppM Int64
postProject = liftM fromSqlKey . runDb . insert



