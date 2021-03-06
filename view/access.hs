{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module View.Access where

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
  import Happstack.Server.FileServe
  import System.Log.Logger
  import Model.User
  import View.Application

  createRegisterForm :: String -> String -> ServerPart Response
  createRegisterForm post_url error_message = ok $ toResponse $ basicTemplate "Haster!" [] $ do
    H.div (H.h1 "Register to haster") 
    H.form H.! A.enctype "multipart/form-data" H.! A.class_ "form-horizontal" 
      H.! A.method "POST"
      H.! A.action (stringValue post_url) $ do
        H.p (H.toHtml error_message)
        H.div H.! A.class_ "row" $ do
          H.div H.! A.class_ "large-12 columns margined" $ do
            H.label "Username"
            H.br
            H.input H.! A.type_ "text" H.! A.name "username"
          H.div H.! A.class_ "large-12 columns margined" $ do
            H.label "Password"
            H.br
            H.input H.! A.type_ "password" H.! A.name "password"
          H.div H.! A.class_ "large-12 columns margined" $ do
            H.label "Password confirmation"
            H.br
            H.input H.! A.type_ "password" H.! A.name "password_confirmation"
          H.div H.! A.class_ "large-12 columns margined" $ do
            H.input H.! A.type_ "submit" H.! A.value "Register" H.! A.class_ "button"
    H.a "Back" H.! A.href "/feed"

  createLoginForm :: String -> ServerPart Response
  createLoginForm error_message = ok $ toResponse $ basicTemplate "Haster!" [] $ do
    H.div H.! A.class_ "row" $ do
      H.h1 "Welcome to haster"
      H.h3 "Haskel + Twitter -> Haster"
      H.form H.! A.enctype "multipart/form-data" H.! A.class_ "form-horizontal margined" 
        H.! A.method "POST"
        H.! A.action (stringValue "/login") $ do
          H.p (H.toHtml error_message)
          H.div H.! A.class_ "row" $ do
            H.div H.! A.class_ "large-12 columns margined" $ do
              H.label "Username"
              H.br
              H.input H.! A.type_ "text" H.! A.name "username"
            H.div H.! A.class_ "large-12 columns margined" $ do
              H.label "Password"
              H.br
              H.input H.! A.type_ "password" H.! A.name "password"
            H.div H.! A.class_ "large-12 columns margined" $ do
              H.input H.! A.type_ "submit" H.! A.value "Login" H.! A.class_ "button"
      H.a "Register" H.! A.href "/register" H.! A.class_ "margined"
