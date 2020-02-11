module Hearthstone.API where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty as List
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol as S
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Foreign (Foreign, ForeignError(..), fail, renderForeignError)
import Foreign.Object (fromHomogeneous)
import Hearthcharm.HTTP (GetRequest, Route, getJSON) as H
import Hearthcharm.HTTP (getJSON, post, readToEnd)
import Hearthstone.DTO (Card)
import Hearthstone.DTO as DTO
import Naporitan as N
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (RequestHeaders(..), headers, hostname, path, protocol, responseAsStream) as H
import Simple.JSON (readImpl)
import Simple.JSON as J

newtype ClientID = ClientID String
newtype ClientSecret = ClientSecret String
newtype Token = Token String
derive newtype instance foreignToken :: J.ReadForeign Token
                        
newtype Options a = Options (Array a)
newtype Collectible = Collectible Boolean
instance showCollectible :: Show Collectible where
  show (Collectible b) = if b then "1" else "0"

data SortOrder = Ascending | Descending

instance showSortOrder :: Show SortOrder where
  show Ascending = "asc"
  show Descending = "desc"

instance readForeignSortOrder :: J.ReadForeign SortOrder where
  readImpl f = do
    str :: String <- readImpl f
    case str of
      "asc" -> pure $ Ascending
      "desc" -> pure $ Descending
      _ -> fail <<< ForeignError $ "Invalid sort order" <> str

data Rarity = Common | Free | Rare | Epic | Legendary
instance showRarity :: Show Rarity where
  show Common = "common"
  show Free = "free"
  show Rare = "rare"
  show Epic = "epic"
  show Legendary = "legendary"

data GameMode = Constructed | Battlegrounds
derive instance eqGameMode :: Eq GameMode
instance showGameMOde :: Show GameMode where
  show Constructed = "constructed"
  show Battlegrounds = "battlegrounds"

data Tier = All | Tier Int
instance showTier :: Show Tier where
  show All = "all"
  show (Tier n) = show n

type AuthResponse
  = { access_token :: Token
    , token_type :: String
    , expires_in :: Number
    }

authenticate :: ClientID -> ClientSecret -> Aff AuthResponse
authenticate (ClientID id) (ClientSecret secret) = do
  headers <- liftEffect headers'
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
        buff <- Buffer.fromString (id <> ":" <> secret) Latin1 :: Effect Buffer
        str <- Buffer.toString Base64 buff
        pure { "Authorization": "Basic " <> str
             , "Content-Type": "application/x-www-form-urlencoded" }

type CardsOptions
  = { locale :: S.SProxy "en_US"
    , set :: String
    , "class" :: String
    , manaCost :: Array Number
    , attack :: Array Number
    , health :: Array Number
    , collectible :: Array Collectible 
    , rarity :: Rarity
    , "type" :: String -- Minion, Spell, Hero, etc.
    , minionType :: String -- There is metadata for this
    , keyword :: String
    , textFilter :: String
    , gameMode :: GameMode
    , page :: Int
    , pageSize :: Int
    , sort :: String -- There might not be metadata for this
    , order :: SortOrder
    , tier :: Maybe Tier
    }

type LangOptions
  = { region :: String
    , locale :: S.SProxy "en_US"
    }
    
routes ::
  { cards :: H.Route H.GetRequest CardsOptions DTO.Pages "https://us.api.blizzard.com/hearthstone/cards"
  , metadata :: H.Route H.GetRequest LangOptions Foreign "https://us.api.blizzard.com/hearthstone/metadata"
  }
routes = N.reflectRecordProxy

auth :: Token -> { "Accept" :: String , "Authorization" :: String }                          
auth (Token token) =
  { "Authorization": "Bearer " <> token
  , "Accept": "application/json"
  }


getImage :: Card -> String
getImage card = case card.battlegrounds of
  Just { image } -> image
  _ -> card.image

cards :: Token -> String -> GameMode -> Aff DTO.Pages
cards token searchTerm gameMode = do
  getJSON
    routes.cards
    case gameMode of
      Battlegrounds -> 
        { locale: S.SProxy
        , pageSize: 1
        , textFilter: searchTerm
        , collectible: []
        , tier: Just All
        , gameMode
        }
      Constructed ->
        { locale: S.SProxy
        , pageSize: 1
        , textFilter: searchTerm
        , collectible: [Collectible true]
        , tier: Nothing
        , gameMode
        }
        
    (auth token)

metadata :: Token -> Aff Foreign
metadata token =
  H.getJSON
    routes.metadata
    { locale: S.SProxy, region: "us" }
    (auth token) 
