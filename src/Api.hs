{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either   (EitherT, left)
import Network.Wai                  (Application)
import Database.Persist.Postgresql  (selectList, Entity(..), (==.)
                                    , fromSqlKey)
import Data.Int                     (Int64)
import Servant

import Config    (Config(..))
import Models -- (Person, runDb, userToPerson, EntityField(UserName))


type PotteryAPI =
    :<|> "projects" :> Get '[JSON] [PotteryProject]
    :<|> "project"  :> ReqBody '[JSON] PotteryProject :> Post '[JSON] PotteryProject
    :<|> "project"  :> Capture "id" Integer :> Get '[JSON] PotteryProject



type AppM = ReaderT Config (EitherT ServantErr IO)

potteryAPI :: Proxy PotteryAPI
potteryAPI = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> ServantErr PotteryAPI
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve potteryAPI (readerServer cfg)

server :: ServerT PotteryAPI AppM
server = getProjects :<|> postProject :<|> getProject



