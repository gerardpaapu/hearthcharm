module Slack.API where

import Prelude

import Data.Maybe (Maybe)
import Data.Options ((:=))
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign (ForeignError(..), fail)
import Foreign.Object (fromHomogeneous)
import Hearthcharm.HTTP (post, readToEnd)
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

postMessage :: BotToken -> String -> String -> Aff Unit
postMessage (BotToken token) channel text = do
  resp <- post body $ mempty
    <> H.protocol := "https:"
    <> H.hostname := "slack.com"
    <> H.path := "/api/chat.postMessage"
    <> H.headers := H.RequestHeaders (fromHomogeneous h)
  rt <- readToEnd UTF8 (H.responseAsStream resp)
  liftEffect $ Console.log rt
  pure unit
    where
    body = J.writeJSON { channel, text }
    h = { "Content-Type": "application/json; charset=utf-8"
        , "Content-Length": Buffer.byteLength body UTF8 # show
        , "Authorization": "Bearer " <> token
        }

type EventWrapper r
   = { token :: VerificationToken
     , type :: String
     , challenge :: Maybe String
     , event :: Maybe r
     }

data EventBody
   = Message { channel :: String
             , text :: String
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
