module Hearthstone.API where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty as List
import Data.Options ((:=))
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Foreign (renderForeignError)
import Foreign.Object (fromHomogeneous)
import Hearthcharm.HTTP (getJSON, post, readToEnd)
import Hearthstone.DTO as DTO
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as H
import Simple.JSON as J

newtype ClientID = ClientID String
newtype ClientSecret = ClientSecret String
newtype Token = Token String
derive newtype instance foreignToken :: J.ReadForeign Token

type AuthResponse
  = { access_token :: Token
    , token_type :: String
    , expires_in :: Number
    }

authenticate :: ClientID -> ClientSecret -> Aff AuthResponse
authenticate (ClientID id) (ClientSecret secret) = do
  headers <- liftEffect $ headers'
  resp <- post "grant_type=client_credentials" $ mempty
          <> H.protocol := "https:"
          <> H.hostname := "us.battle.net"
          <> H.path := "/oauth/token"
          <> H.headers := H.RequestHeaders (fromHomogeneous headers)
  json <- readToEnd UTF8 (H.responseAsStream resp)
  case J.readJSON json of
    Left err -> throwError (err # List.head # renderForeignError # error)
    Right o -> pure o
    where
      headers' = do
        buff <- Buffer.fromString (id <> ":" <> secret) Latin1
        str <- Buffer.toString Base64 buff
        pure { "Authorization": "Basic " <> str
             , "Content-Type": "application/x-www-form-urlencoded" }

cards :: Token -> String -> Aff DTO.Pages
cards (Token token) searchTerm = do
  getJSON url query headers
  where
    url = "https://us.api.blizzard.com/hearthstone/cards"
    query = { local: "en_US", pageSize: 1, textFilter: searchTerm }
    headers =
      { "Authorization": "Bearer " <> token
      , "Accept": "application/json" }
