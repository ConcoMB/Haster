{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Controller.Access where

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
                        CookieLife(Session), addCookie, mkCookie, HasRqData, expireCookie)
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
import View.Access
import Model.User
import Acid

authorize :: AcidState Users -> (String -> ServerPart Response) -> ServerPart Response
authorize acid errorHandler = do
     d <- getDataFn authCreds
     case d of
         (Left e) -> errorHandler (Prelude.unlines e)
         (Right (AuthCredentials user pass)) ->  do 
            exists <- query' acid (UserExists user pass)
            if exists
            then do 
              addCookie Session (mkCookie "cookie_user" user)
              addCookie Session (mkCookie "cookie_password" pass)
              return (redirect 302 ("/feed" :: String) (toResponse ()))
            else do
              errorHandler "Invalid username or password"

register :: ServerPart Response
register = createRegisterForm "/register" ""

doRegister :: AcidState Users -> ServerPart Response
doRegister acid =
   do reg <- getDataFn registrationRq
      case reg of
        Left e -> badRequest (toResponse (Prelude.unlines e))
        Right (Registration username password password_confirmation) 
                  | validRegistration (Registration username password password_confirmation) ->
                    do (User user_id username password) <- update' acid (AddUser username password)
                       addCookie Session (mkCookie "cookie_user" username)
                       addCookie Session (mkCookie "cookie_password" password)
                       return (redirect 302 ("/feed" :: String) (toResponse ()))
                  | otherwise -> createRegisterForm  "/register" "User and password can not be empty and passwords must match"

login :: ServerPart Response
login = createLoginForm ""

logout :: ServerPart Response
logout = do
  expireCookie "cookie_user"
  expireCookie "cookie_password"
  return (redirect 302 ("login" :: String) (toResponse ()))

doLogin :: AcidState Users -> ServerPart Response
doLogin acid = authorize acid createLoginForm

data Registration = Registration { r_username :: String, r_password :: String, r_password_confirmation :: String }

registrationRq :: RqData Registration
registrationRq = do
          r_username <- look "username"
          r_password <- look "password"
          r_password_confirmation <- look "password_confirmation"
          return (Registration r_username r_password r_password_confirmation)

validRegistration :: Registration -> Bool
validRegistration (Registration [] b c) = False
validRegistration (Registration a [] c) = False
validRegistration (Registration a b []) = False
validRegistration (Registration a b c) = b == c

data AuthCredentials = AuthCredentials { username :: String, password :: String }

authCreds :: RqData AuthCredentials
authCreds = do
  username <- look "username"
  password <- look "password"
  return (AuthCredentials username password)
