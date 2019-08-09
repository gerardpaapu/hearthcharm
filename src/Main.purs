module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, error, forkAff, runAff_, throwError)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Dotenv as Dotenv
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Uncurried as E
import Hearthcharm.HTTP as HTTP
import Hearthcharm.Util (orThrow)
import Hearthstone.API as HS
import Node.Process as Process
import Simple.JSON as J
import Slack.API (EventBody(..))
import Slack.API as Slack

main = launchAff_ do
  _ <- Dotenv.loadFile
  clientID <- HS.ClientID <$> getEnv "HEARTHSTONE_CLIENT_ID"
  clientSecret <- HS.ClientSecret <$> getEnv "HEARTHSTONE_CLIENT_SECRET"
  auth <- HS.authenticate clientID clientSecret
  pagesFrom auth.access_token 1 
  where
    getPage token n =
        HTTP.getJSON
            HS.routes.cards
            { locale: SProxy, pageSize: 200, page: n }
            (HS.auth token)


    pagesFrom token n = do
      page <- getPage token n
      page.cards # traverse_ \card ->
        log $ (show card.name) <> "," <> (show card.slug)
      if page.pageCount > page.page then
        pagesFrom token (n + 1)
      else
        pure unit

type Response
  = { statusCode :: Number
    , body :: String
    , headers :: { "Content-Type" :: String }
    }

type LambdaEvent
  = { body :: String }

type LambdaResponse
  = { statusCode :: Number
    , body :: String
    , headers ::  { "Content-Type" :: String }
    }

foreign import data LambdaContext :: Type
foreign import callbackWaitsForEmptyLoop :: Boolean -> LambdaContext -> Effect Unit

type LambdaCallback = E.EffectFn2 (Nullable Error) LambdaResponse Unit
type LambdaHandler = E.EffectFn3 LambdaEvent LambdaContext LambdaCallback Unit

handler :: LambdaHandler
handler = E.mkEffectFn3 $ \event context cb_ -> do
  let cb = E.runEffectFn2 cb_
  context # callbackWaitsForEmptyLoop false
  runAff_
    (case _ of
       Left err ->
         cb null { statusCode: 500.0
                 , body: "Oops!"
                 , headers: { "Content-Type": "plain/text" }
                 }
       Right v  ->
         cb null v)
    (handle event)
  where
    handle ev = do
      params :: Slack.Event <- J.readJSON ev.body # orThrow "Invalid request body"
      if params.type == "url_verification" then do
        let body = J.writeJSON { challenge: params.challenge }
        pure $ { statusCode: 200.0
               , body
               , headers: { "Content-Type": "application/json" }
               }
      else do
        verificationToken <- Slack.VerificationToken <$> getEnv "VERIFICATION_TOKEN"
        if params.token /= verificationToken then
          throwError (error "Mismatched Verification Token")
        else do
          case params.event of
            Just (Message msg) ->
              void $ forkAff $ handleSlackMessage msg

            otherwise -> pure unit

          pure $ { statusCode: 200.0
                 , body: ""
                 , headers: { "Content-Type": "application/json" }
                 }

handleSlackMessage ::
  { channel :: Slack.Channel
  , text :: String
  , thread_ts :: Maybe Slack.Thread
  , ts :: Slack.Thread
  }
  -> Aff Unit
handleSlackMessage { text, thread_ts, ts, channel } = do
  clientID <- HS.ClientID <$> getEnv "HEARTHSTONE_CLIENT_ID"
  clientSecret <- HS.ClientSecret <$> getEnv "HEARTHSTONE_CLIENT_SECRET"

  token <- HS.authenticate clientID clientSecret
  searchResults <- parseMessage text # parTraverse \term -> do
    results <- HS.cards token.access_token term
    pure $ { result: results.cards !! 0 , term }

  let thread =
        case searchResults of
          [_] -> thread_ts
          _ -> Just ts

  botToken <- Slack.BotToken <$> getEnv "BOT_TOKEN"
  searchResults # traverse_ \item ->
    case item.result of
      Just card ->
        let block = Slack.imageBlock card.name card.image
        in Slack.postMessage botToken channel thread card.name (Just [block])

      Nothing ->
        let msg = "No results for: " <> item.term
        in Slack.postMessage botToken channel thread msg Nothing

foreign import parseMessage :: String -> Array String

getEnv :: String -> Aff String
getEnv a =
  Process.lookupEnv a
    # liftEffect
    >>= orThrow ("missing env variable " <> a)

log :: String -> Aff Unit
log = liftEffect <<< Console.log
