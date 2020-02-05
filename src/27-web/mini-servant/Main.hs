{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API
  ( (:<|>) (..),
    (:>),
    FormUrlEncoded,
    Get,
    Post,
    --QueryParam',
    ReqBody,
  )
import Servant.HTML.Blaze (HTML)
import Servant.Server (Handler)
import qualified Servant.Server as Server
import Text.Blaze ((!), Markup)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 (AttributeValue)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData)
import Prelude hiding (id)

data User = User {name :: Text, userId :: Int}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm, FromHttpApiData)

type API =
  Get '[HTML] Markup
    :<|> ( "user"
             :> ReqBody '[FormUrlEncoded] User
             :> Post '[HTML] Markup
         )

server :: Handler Markup :<|> (User -> Handler Markup)
server =
  index
    :<|> createUser

index :: Handler Markup
index = do
  pure (page userForm)

userForm :: Html.Html
userForm =
  Html.div ! Attr.class_ "row" $ do
    form "/user" "post" $ do
      field "name"
      field "userId"
      submit "Create user"

form ::
  AttributeValue ->
  AttributeValue ->
  Html.Html ->
  Html.Html
form action method html =
  Html.div ! Attr.class_ "col-md-4" $ do
    Html.form
      ! Attr.action action
      ! Attr.method method
      ! Attr.class_ "border m-3 p-2 bg-light"
      $ html

submit :: Html.Html -> Html.Html
submit label =
  Html.button
    ! Attr.type_ "submit"
    ! Attr.class_ "btn btn-primary"
    $ label

field :: AttributeValue -> Markup
field name = do
  Html.div ! Attr.class_ "form-group" $ do
    Html.input
      ! Attr.type_ "text"
      ! Attr.class_ "form-control form-control-sm"
      ! Attr.name name
      ! Attr.placeholder name

page :: Markup -> Markup
page body = do
  Html.html do
    Html.head do
      Html.title "Example App"
      Html.link
        ! Attr.rel "stylesheet"
        ! Attr.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    Html.body do
      Html.h1
        ! Attr.class_ "display-4 text-center"
        $ "Example App"
      Html.div ! Attr.class_ "container" $ do
        Html.div ! Attr.class_ "row" $ do
          Html.div ! Attr.class_ "col-md-6" $ body

createUser :: User -> Handler Markup
createUser user@User {..} = do
  liftIO (print user)
  pure $ page $ do
    Html.p ("Id: " <> toHtml userId)
    Html.p ("Username: " <> toHtml name)

main :: IO ()
main = do
  putStrLn "Running Server"
  let application = Server.serve @API Proxy server
  Warp.run 8000 application
