module Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Parallel (class Parallel, parTraverse)
import Control.Promise (Promise, fromAff)
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, for_)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Uncurried as E
import Hearthstone.API as HS
import Node.Process as Process
import Simple.JSON as J
import Slack.API (EventBody(..))
import Slack.API as Slack


inParallel :: forall f eff par a b. Parallel par eff => Traversable f => f a -> (a -> eff b) -> eff (f b)
inParallel a b = parTraverse b a

type Response
  = { statusCode :: Number
    , body :: String
    , headers :: { "Content-Type" :: String }
    }

type LambdaEvent
  = { body :: String }

handler :: E.EffectFn1 LambdaEvent (Promise Response)
handler = E.mkEffectFn1 $ \ev -> fromAff $ handle ev
  where
    handle ev = do
      clientID <- HS.ClientID <$> getEnv "HEARTHSTONE_CLIENT_ID"
      clientSecret <- HS.ClientSecret <$> getEnv "HEARTHSTONE_CLIENT_SECRET"
      verificationToken <- Slack.VerificationToken <$> getEnv "VERIFICATION_TOKEN"
      botToken <- Slack.BotToken <$> getEnv "BOT_TOKEN"

      params :: Slack.Event <- J.readJSON ev.body # orThrow "Invalid request body"

      if params.token /= verificationToken then
        throwError (error "Mismatched Verification Token")
      else if params.type == "url_verification" then do
        let body = J.writeJSON { challenge: params.challenge }
        pure $ { statusCode: 200.0
               , body
               , headers: { "Content-Type": "application/json" }
               }
      else do
        case params.event of
          Just (Message { channel, text, ts, thread_ts }) -> do
            token <- HS.authenticate clientID clientSecret
            searchResults <- inParallel (parseMessage text) $ \term -> do
                results <- HS.cards token.access_token term
                pure $ { result: results.cards !! 0
                       , term
                       }

            let thread =
                  if Array.length searchResults > 1 then
                    Just ts
                  else
                    thread_ts

            for_ searchResults $ \item ->
              case item.result of
                Just card ->
                  let block = Slack.imageBlock card.name card.image
                  in Slack.postMessage botToken channel thread card.name (Just [block])

                Nothing ->
                  let msg = "No results for: " <> item.term
                  in Slack.postMessage botToken channel thread msg  Nothing
          _ -> pure unit

        pure $ { statusCode: 200.0
               , body: ""
               , headers: { "Content-Type": "application/json" }
               }

foreign import parseMessage :: String -> Array String


getEnv :: String -> Aff String
getEnv a =
  Process.lookupEnv a
    # liftEffect
    >>= orThrow ("missing env variable " <> a)

orThrow :: forall t3 t6 t7. Foldable t3 => Applicative t6 => MonadThrow Error t6 => String -> t3 t7 -> t6 t7
orThrow e m =
  foldl (\_ v -> pure v) (throwError (error e)) m


log :: String -> Aff Unit
log = liftEffect <<< Console.log
