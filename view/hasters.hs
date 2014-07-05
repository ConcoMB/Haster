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
  import Model.Haster
  import View.Application

  createHaster :: Haster -> String -> String -> ServerPart Response
  createHaster (Haster (HasterId key) text) post_url error_message = ok $ toResponse $
      basicTemplate "Haster!" [] $ do
        H.div (H.h1 "New Haster") H.! A.class_ "page-header"
        H.form H.! A.enctype "multipart/form-data" H.! A.class_ "form-horizontal" 
          H.! A.method "POST"
          H.! A.action (stringValue post_url) $ do
            H.p (H.toHtml error_message)
            H.div H.! A.class_ "control-group" $ do
              H.div H.! A.class_ "controls" $ do  
                H.textarea H.! A.style "resize:none" 
                           H.! A.type_ "text" 
                           H.! A.name "haster_text"
                           H.! A.maxlength (H.toValue (140 ::Integer))  
                           H.! A.cols (H.toValue (50 ::Integer)) 
                           H.! A.rows (H.toValue (6 ::Integer)) $ (H.toHtml text)
            H.div H.! A.class_ "control-group" $ do
              H.div H.! A.class_ "controls" $ do  
                H.input H.! A.type_ "hidden" H.! A.name "haster_id" H.! A.value (stringValue (show key))
                H.input H.! A.type_ "submit" H.! A.value "Hast!" H.! A.class_ "button"
        H.a "Back to feed" H.! A.href "/feed" H.! A.class_ "button"


  buildShowResponse :: Haster -> ServerPart Response
  buildShowResponse (Haster key text) = 
    ok (toResponse (
          basicTemplate "Haster!"
            []
            (do H.div ( H.h1 "Look at my haster") H.! A.class_ "page-header"
                H.div H.! A.class_ "hero-unit" $ do
                  H.p (H.toHtml text)
                buildDeleteLink key
                H.a "Back" H.! A.href "/feed" H.! A.class_ "button"
            )
      ))

  buildDeleteLink :: HasterId -> H.Html
  buildDeleteLink (HasterId key) = H.form
                                  H.! A.method "POST"
                                  H.! A.action (stringValue ("/hasters/delete/" ++ show key)) $ do
                                      H.input H.! A.type_ "submit" H.! A.value "Delete" H.! A.class_ "button alert"

  buildResponse :: [Haster] -> ServerPart Response
  buildResponse hasters = 
    ok (toResponse (
          basicTemplate "Haster!" [] $ do
            H.div (H.h1 "Hasters") H.! A.class_ "page-header"
            if Prelude.null hasters
            then
              H.h2 "No hasters"
            else
              forM_ hasters (H.div . (\(Haster key text) -> H.a H.! (buildLink key) $ H.toHtml text)) H.! A.class_ "haster"

      ))

  buildLink :: HasterId -> H.Attribute
  buildLink (HasterId key) = A.href (stringValue ("/hasters/" ++ (show key)))

