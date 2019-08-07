module Slack.API where

import Prelude

import Data.Maybe (Maybe)
import Data.Options ((:=))
import Effect.Aff (Aff)
import Foreign.Object (fromHomogeneous)
import Hearthcharm.HTTP (post)
import Node.Encoding (Encoding(..))
import Node.Encoding as Buffer
import Node.HTTP.Client as H
import Simple.JSON (class ReadForeign, readImpl)
import Simple.JSON as J


newtype BotToken = BotToken String
newtype VerificationToken = VerificationToken String
derive newtype instance verificationTokenEq :: Eq VerificationToken
derive newtype instance readForeignToken :: J.ReadForeign VerificationToken
derive newtype instance showToken :: Show VerificationToken

newtype Thread = Thread String
derive newtype instance readForeignThread :: J.ReadForeign Thread
derive newtype instance writeForeignThread :: J.WriteForeign Thread

newtype Channel = Channel String
derive newtype instance readForeignChannel :: J.ReadForeign Channel
derive newtype instance writeForeignChannel :: J.WriteForeign Channel


postMessage :: BotToken -> Channel -> Maybe Thread -> String -> Blocks -> Aff Unit
postMessage (BotToken token) (Channel channel) thread text blocks = do
  resp <- post body $ mempty
    <> H.protocol := "https:"
    <> H.hostname := "slack.com"
    <> H.path := "/api/chat.postMessage"
    <> H.headers := H.RequestHeaders (fromHomogeneous h)
  pure unit
    where
    body = J.writeJSON { channel, text, blocks, thread_ts: thread }
    h = { "Content-Type": "application/json; charset=utf-8"
        , "Content-Length": Buffer.byteLength body UTF8 # show
        , "Authorization": "Bearer " <> token
        }

type Block
  = { type :: String
    , title :: { type :: String
               , text :: String
               , emoji :: Boolean
               }
    , image_url :: String
    , alt_text :: String
    }

type Blocks = Maybe (Array Block)

imageBlock :: String -> String -> Block
imageBlock title url =
  { type: "image"
  , title: { type: "plain_text"
           , text: title
           , emoji: false
           }
  , image_url: url
  , alt_text: title
  }

type EventWrapper r
   = { token :: VerificationToken
     , type :: String
     , challenge :: Maybe String
     , event :: Maybe r
     }

data EventBody
   = Message { channel :: Channel
             , text :: String
             , ts :: Thread
             , thread_ts :: Maybe Thread
             }
   | Unknown


type Event = EventWrapper EventBody

instance readForeignEventBody ::
  ReadForeign EventBody where
  readImpl f = do
    tagged :: { type :: String } <- readImpl f
    case tagged.type of
      "message" -> Message <$> readImpl f
      _ -> pure $ Unknown

-- {
--         "token": "z26uFbvR1xHJEdHE1OQiO6t8",
--         "team_id": "T061EG9RZ",
--         "api_app_id": "A0FFV41KK",
--         "event": {
--                 "type": "reaction_added",
--                 "user": "U061F1EUR",
--                 "item": {
--                         "type": "message",
--                         "channel": "C061EG9SL",
--                         "ts": "1464196127.000002"
--                 },
--                 "reaction": "slightly_smiling_face",
--                 "item_user": "U0M4RL1NY"
--                 "event_ts": "1465244570.336841"
--         },
--         "type": "event_callback",
--         "authed_users": [
--                 "U061F7AUR"
--         ],
--         "event_id": "Ev9UQ52YNA",
--         "event_time": 1234567890
-- }
