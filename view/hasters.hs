{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module View.Hasters where

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
import Model.Haster
import View.Application

buildShowResponse :: Haster -> String -> ServerPart Response
buildShowResponse (Haster key text username) logged_username = ok $ toResponse $ basicTemplate "Haster!" [] $ do 
  H.div (H.h1 (H.toHtml ("Look at " ++ username ++ "'s haster")))
  H.div $ do
    H.h4 (H.toHtml text)
  showButtons key username logged_username
  H.a "Back" H.! A.href "/feed"

showButtons :: HasterId -> String -> String -> H.Html
showButtons (HasterId key) haster_user logged_user = 
  if haster_user == logged_user
  then 
    H.form
      H.! A.method "POST"
      H.! A.action (stringValue ("/hasters/delete/" ++ show key)) $ do
          H.input H.! A.type_ "submit" H.! A.value "Delete" H.! A.class_ "button alert"
  else
    H.form
      H.! A.method "POST"
      H.! A.action (stringValue ("/hasters/rehast/" ++ show key)) $ do
          H.input H.! A.type_ "submit" H.! A.value "Rehast" H.! A.class_ "button"

createHaster :: Haster -> String -> String -> ServerPart Response
createHaster (Haster (HasterId key) text username) post_url error_message = ok $ toResponse $ basicTemplate "Haster!" [] $ do
  H.div (H.h1 "New Haster")
  H.form H.! A.enctype "multipart/form-data" H.! A.method "POST" H.! A.action (stringValue post_url) $ do
      H.p (H.toHtml error_message)
      H.div H.! A.class_ "row" $ do
        H.div H.! A.class_ "columns large-12 margined" $ do  
          H.textarea H.! A.style "resize:none" H.! A.type_ "text" H.! A.name "haster_text" H.! A.maxlength (H.toValue (140 ::Integer))  
                     H.! A.cols (H.toValue (50 ::Integer)) H.! A.rows (H.toValue (6 ::Integer)) $ (H.toHtml text)
      H.div H.! A.class_ "row" $ do
        H.div H.! A.class_ "columns large-12 margined" $ do  
          H.input H.! A.type_ "hidden" H.! A.name "haster_id" H.! A.value (stringValue (show key))
          H.input H.! A.type_ "submit" H.! A.value "Hast!" H.! A.class_ "button"
  H.a "Back to feed" H.! A.href "/feed"

buildFeed :: [Haster] -> ServerPart Response
buildFeed hasters = ok $ toResponse $ basicTemplate "Haster!" [] $ do
  H.div (H.h1 "All Hasters") H.! A.class_ "page-header"
  H.a "Hast it up!" H.! A.href "/new_haster" H.! A.class_ "button"
  if Prelude.null hasters
  then
    H.h3 "No hasters"
  else
    forM_ hasters (\(Haster (HasterId key) text username) -> 
      H.a H.! A.href (stringValue ("/hasters/" ++ (show key))) $ do
        H.div H.! A.class_ "haster" $ do
          H.strong (H.toHtml (username ++ " hasted:"))
          H.p (H.toHtml text)) 
