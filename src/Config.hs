{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Config where

import           Control.Exception                    (throwIO)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)

data Config = Config
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }

data Environment =
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined
    , getEnv  = Development
    }

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test =
    runNoLoggingT (createPostgresqlPool (connStr "test") (envPool Test))
makePool Development =
    runStdoutLoggingT (createPostgresqlPool (connStr "dev") (envPool Development))
makePool Production = do
    pool <- runMaybeT $ do
        let keys = [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = mconcat . zipWith (<>) keys $ BS.pack <$> envVars
        runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
    case pool of
         Nothing -> throwIO (userError "Database Configuration not present in environment.")
         Just a -> return a

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=potteryapp_" <> sfx <> " user=anna password='' port=5432"
