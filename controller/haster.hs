{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Controller.Haster where

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
import System.Log.Logger
import View.Hasters
import Model.Haster
import Acid

doNewHaster :: AcidState Hasters -> ServerPart Response
doNewHaster acid = createHaster (Haster (HasterId 0) "" "") "/create_haster" ""

doCreateHaster :: AcidState Hasters -> ServerPart Response
doCreateHaster acid =
   do post_data <- getDataFn hasterRq
      case post_data of
        Left e -> badRequest (toResponse (Prelude.unlines e))
        Right (Haster (HasterId haster_id) haster_text username) 
                  | isValidHaster (Haster (HasterId haster_id) haster_text username) ->
                    do (Haster (HasterId haster_id) haster_text username) <- update' acid (AddHaster haster_text username)
                       return (redirect 302 ("/feed" :: String) (toResponse ()))
                  | otherwise -> createHaster (Haster (HasterId 0) "" "") "/create_post" "Text cannot be empty dude"

doShowHaster :: AcidState Hasters -> Integer -> ServerPart Response
doShowHaster acid haster_id = 
   do d <- getDataFn userCookieRq
      case d of
        Left e -> badRequest (toResponse (Prelude.unlines e))
        Right (UserCookie u) ->
           do haster <- query' acid (GetHaster (HasterId haster_id))
              case haster of
                Just haster ->  do buildShowResponse haster u
                Nothing -> badRequest (toResponse (("Could not find haster with id " ++ show haster_id) :: String))

doDeleteHaster :: AcidState Hasters -> Integer -> ServerPart Response
doDeleteHaster acid haster_id = 
  do to_delete <- query' acid (GetHaster (HasterId haster_id))
     case to_delete of
      Just h -> do 
                  update' acid (DeleteHaster h)
                  return (redirect 302 ("/feed" :: String) (toResponse ()))
      Nothing -> badRequest (toResponse (("Could not find haster with id " ++ show haster_id) :: String))

doFeed :: AcidState Hasters -> ServerPart Response
doFeed acid = 
  do hasters <- query' acid Feed
     buildFeed hasters

doRehast :: AcidState Hasters -> Integer -> ServerPart Response
doRehast acid haster_id = 
  do rehast <- query' acid (GetHaster (HasterId haster_id))
     case rehast of
      Just (Haster (HasterId id) text user_a) -> 
        do d <- getDataFn userCookieRq
           case d of
              Left e -> badRequest (toResponse (Prelude.unlines e))
              Right (UserCookie u) ->
                do (Haster (HasterId id) text u) <- update' acid (AddHaster text u)
                   return (redirect 302 ("/feed" :: String) (toResponse ()))
      Nothing -> badRequest (toResponse (("Could not find haster with id " ++ show haster_id) :: String))

hasterRq :: RqData Haster
hasterRq = do
  text <- look "haster_text"
  hasterId <- lookRead "haster_id"
  username <- lookCookieValue "User"
  return (Haster (HasterId hasterId) text username)

data UserCookie = UserCookie { user_name :: String }

userCookieRq :: RqData UserCookie
userCookieRq = do
  user_name <- lookCookieValue "cookie_user"
  return (UserCookie user_name)
