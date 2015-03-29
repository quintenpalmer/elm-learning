{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Network.HTTP as HTTP

import Data.Text as Text
import qualified Snap.Core as SnapC
import Snap.Core (Snap)
import qualified Snap.Http.Server as SnapS
import qualified Snap.Util.FileServe as SnapF

import Data.Map (Map, fromList)
import Data.Aeson (encode, ToJSON)

main :: IO ()
main = SnapS.httpServe (SnapS.setPort 8080 SnapS.defaultConfig) site

site :: Snap ()
site =
    --Snap.ifTop (snapHtml Html.indexHtml) <|>
    SnapC.route [
        ("Home.elm", redirectToElmReactor),
        ("rest/v1/artists", SnapC.method SnapC.GET getArtistsHandler)
    ] <|>
    SnapC.dir "static" (SnapF.serveDirectory "static")

redirectToElmReactor :: Snap ()
redirectToElmReactor = do
    bs <- liftIO $ HTTP.simpleHTTP (HTTP.getRequest "http://127.0.0.1:8000/Home.elm") >>= HTTP.getResponseBody
    SnapC.modifyResponse $ SnapC.setContentType "text/html"
    SnapC.writeText $ Text.pack bs

getArtistsHandler :: Snap ()
getArtistsHandler = do
    SnapC.modifyResponse $ SnapC.setContentType "application/json"
    SnapC.writeLBS $ encode (
        fromList [("name", "Nero")] :: Map String String,
        fromList [("name", "XX")] :: Map String String,
        fromList [("name", "Passion Pit")] :: Map String String)

withData :: ToJSON a => a -> Map String a
withData val = fromList [("data", val)]
