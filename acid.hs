{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Acid where

import Control.Applicative  ((<$>), (<*>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader ( ask )
import Control.Exception    ( bracket )
import Happstack.Server(Method(GET, HEAD, POST, DELETE), dir, methodM, ServerPart, Response,
                        toResponse, simpleHTTP, nullConf, ok, toMessage, look, lookRead,
                        defaultBodyPolicy, BodyPolicy, decodeBody, RqData,
                        getDataFn, badRequest, lookFile, path, resp, seeOther, method,
                        getHeaderM, unauthorized, setHeaderM, askRq, getHeader, lookCookieValue,
                        CookieLife(Session), addCookie, mkCookie, HasRqData)
import           Text.Blaze
import           Text.Blaze.Internal
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import System.IO
import System.Log.Logger ( updateGlobalLogger
                         , rootLoggerName
                         , setLevel
                         , Priority(..)
                         )
import Happstack.Server.Internal.Types
import Data.ByteString
import Data.ByteString.Base64 as Base64
import qualified Data.Text as B
import Data.ByteString.Char8 as C
import Data.Text.Encoding
import Data.Map as M
import Data.Data            ( Data, Typeable )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
                            , Proxy(..), getOne, ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Happstack.Server.FileServe
import Model.Haster
import Model.User

$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Users)

addUser :: String -> String -> Update Users User
addUser new_username new_password =
    do b@Users{..} <- get
       let user = User { userId = nextUserId
                         , username  = new_username
                             , password = new_password
                           }
       put $ b { nextUserId = succ nextUserId
               , users      = IxSet.insert user users
               }
       return user

getUser :: String -> Query Users (Maybe User)
getUser username = 
    do Users{..} <- ask
       return (getOne (users @= username))

userExists :: String -> String -> Query Users Bool
userExists username password = do
                                Users{..} <- ask
                                let all_users = IxSet.toList users
                                return (Prelude.foldr (\(User key user pass) rec -> 
                                  (username == user && pass == password) || rec) False all_users)


$(makeAcidic ''Users ['addUser, 'getUser, 'userExists])


$(deriveSafeCopy 0 'base ''HasterId)
$(deriveSafeCopy 0 'base ''Hasters)
$(deriveSafeCopy 0 'base ''Haster)

getHaster :: HasterId -> Query Hasters (Maybe Haster)
getHaster key = 
    do Hasters{..} <- ask
       return (getOne (hasters @= key))

feed :: Query Hasters [Haster]
feed = do
             Hasters{..} <- ask
             let h = IxSet.toList hasters
             return (Prelude.reverse h)

addHaster :: String -> String -> Update Hasters Haster
addHaster haster_text haster_username =
    do b@Hasters{..} <- get
       let haster = Haster { hasterId = nextHasterId
                             , text  = haster_text
                             , username = haster_username
                           }
       put $ b { nextHasterId = succ nextHasterId
               , hasters      = IxSet.insert haster hasters
               }
       return haster

deleteHaster :: Haster -> Update Hasters ()
deleteHaster h = do
  b@Hasters{..} <- get
  put $ b { hasters = IxSet.delete h hasters}

$(makeAcidic ''Hasters ['feed, 'getHaster, 'addHaster, 'deleteHaster])


