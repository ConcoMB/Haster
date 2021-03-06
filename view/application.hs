{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module View.Application where

import Control.Applicative ((<$>), optional)
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
import qualified Data.IxSet as IxSet
import Happstack.Server.FileServe
import System.Log.Logger

basicTemplate :: String -> [H.Html] -> H.Html -> H.Html
basicTemplate title headers body =
    H.html $ do
      H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/static/css/application.css"
      H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/static/css/foundation.css"
      H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "http://fonts.googleapis.com/css?family=Pacifico"
      H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "http://fonts.googleapis.com/css?family=Architects+Daughter"
      H.head $ do
        H.title (H.toHtml title)
        H.meta H.! A.httpEquiv "Content-Type" H.! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        H.div H.! A.class_ "header" $ do
          H.div H.! A.class_ "row" $ do
            H.div H.! A.class_ "columns large-3" $ do
              H.a  H.! A.href "/feed" $ do
                H.h1 "Haster" H.! A.class_ "title"
            H.div H.! A.class_ "columns large-3 float-right small-offset-6" $ do
              H.a "Logout" H.! A.class_ "logout" H.! A.href "logout" 
        H.div H.! A.class_ "row" $ do
          body
