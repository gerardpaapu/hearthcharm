module Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Parallel (parTraverse)
import Control.Promise (Promise, fromAff)
import Data.Array ((!!))
import Data.Compactable (compact)
import Data.Foldable (class Foldable, foldl, intercalate)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Uncurried as E
import Hearthstone.API as HS
import Node.Process as Process
import Simple.JSON as J
import Slack.API (EventBody(..))
import Slack.API as Slack


inParallel a b = parTraverse b a

type Response
  = { statusCode :: Number
    , body :: String
    , headers :: { "Content-Type" :: String }
    }

type LambdaEvent
  = { body :: String
    }


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
          Just (Message { channel, text }) -> do
            token <- HS.authenticate clientID clientSecret
            images <- inParallel (parseMessage text) $ \match -> do
                results <- HS.cards token.access_token match
                pure $ _.image <$> results.cards !! 0

            let lines = intercalate "\n" $ compact images
            if lines /= "" then
              Slack.postMessage botToken channel lines
            else
              pure unit
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
