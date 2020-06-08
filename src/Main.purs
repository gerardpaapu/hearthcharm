module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null)
import Data.Traversable (traverse_)
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (Aff, error, forkAff, launchAff_, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Uncurried as E
import Hearthcharm.Util (orThrow)
import Hearthstone.API as HS
import Hearthstone.DTO as DTO
import Node.Process as Process
import Simple.JSON as J
import Slack.API (EventBody(..))
import Slack.API as Slack


main :: Effect Unit
main = launchAff_ do
  _ <- Dotenv.loadFile
  let searchTerm = "Mama Bear"
  clientID <- HS.ClientID <$> getEnv "HEARTHSTONE_CLIENT_ID"
  clientSecret <- HS.ClientSecret <$> getEnv "HEARTHSTONE_CLIENT_SECRET"
  auth <- HS.authenticate clientID clientSecret
  results <- do 
    cs <- HS.cards auth.access_token searchTerm HS.Constructed
    case cs.cardCount of
        0.0 -> HS.cards auth.access_token searchTerm HS.Battlegrounds
        _ -> pure cs
  liftEffect $ logShow $ results
    
  -- pagesFrom auth.access_token 1 
  -- where
    -- getPage token n =
    --     HTTP.getJSON
    --         HS.routes.cards
    --         { locale: SProxy, pageSize: 200, page: n }
    --         (HS.auth token)


    -- pagesFrom token n = do
    --   page <- getPage token n
    --   page.cards # traverse_ \card ->
    --     log $ (show card.name) <> "," <> (show card.slug)
    --   if page.pageCount > page.page then
    --     pagesFrom token (n + 1)
    --   else
    --     pure unit

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
       Left err -> do
         Console.log $ "Server Error: " <> show err
         cb null { statusCode: 500.0
                 , body: "Server Error"
                 , headers: { "Content-Type": "plain/text" }
                 }
       Right v  ->
         cb null v)
    (handle event)
  where
    handle ev = do
      params :: Slack.Event <- case J.readJSON ev.body of
        Left err -> throwError (error $ show err)
        Right v -> pure v
      if params.type == "url_verification" then do
        let body = J.writeJSON { challenge: params.challenge }
        pure $ { statusCode: 200.0
               , body
               , headers: { "Content-Type": "application/json" }
               }
      else do
        verificationToken <- Slack.VerificationToken <$> getEnv "VERIFICATION_TOKEN"
        if params.token /= Just verificationToken then
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


search :: HS.Token -> SearchQuery -> Aff DTO.Pages
search token { term, mode } = case mode of
  Just m  -> HS.cards token term m
  Nothing -> searchWithFallback token term

searchWithFallback :: HS.Token -> String -> Aff DTO.Pages 
searchWithFallback token term = do
  cs <- HS.cards token term HS.Battlegrounds
  case cs.cardCount of
    0.0 -> HS.cards token term HS.Constructed
    _ -> pure cs

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

  { access_token } <- HS.authenticate clientID clientSecret
  searchResults <- parseMessage text # parTraverse \{ term, mode } -> do
    results <- search access_token { term, mode }
    pure $ { result: results.cards !! 0 , term }

  let thread =
        case searchResults of
          [_] -> thread_ts
          _ -> Just ts

  botToken <- Slack.BotToken <$> getEnv "BOT_TOKEN"
  searchResults # traverse_ \item ->
    case item.result of
      Just card ->
        let block = Slack.imageBlock card.name (HS.getImage card)
        in Slack.postMessage botToken channel thread card.name (Just [block])

      Nothing ->
        let msg = "No results for: " <> item.term
        in Slack.postMessage botToken channel thread msg Nothing

type SearchQuery = { mode :: Maybe HS.GameMode, term :: String }
  
foreign import parseMessageImpl
  :: String
  -> (String -> SearchQuery)
  -> (String -> SearchQuery)
  -> (String -> SearchQuery)
  -> Array SearchQuery
  
parseMessage :: String -> Array SearchQuery
parseMessage source =
  parseMessageImpl
     source
     { mode: Nothing, term: _ }
     { mode: Just HS.Battlegrounds, term: _ }
     { mode: Just HS.Constructed, term: _ }

getEnv :: String -> Aff String
getEnv a =
  Process.lookupEnv a
    # liftEffect
    >>= orThrow ("missing env variable " <> a)

log :: String -> Aff Unit
log = liftEffect <<< Console.log
