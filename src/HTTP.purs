module Hearthcharm.HTTP where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (head)
import Data.Options (Options, (:=))
import Effect.Aff (Aff, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Foreign (renderForeignError)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as H
import Node.Stream as S
import Simple.JSON as J



readToEnd :: forall r. Encoding -> S.Readable r -> Aff String
readToEnd enc stream = makeAff go
  where
  go cb = do
    result <- Ref.new ""
    S.onDataString stream enc (\s -> Ref.modify_ (_ <> s) result)
    S.onError stream (\e -> cb (Left e))
    S.onEnd stream (do r <- Ref.read result
                       cb (Right r))
    pure mempty
  
post :: String -> Options H.RequestOptions -> Aff H.Response
post body opt = makeAff go
  where
  go cb = do
    req <- H.request (opt <> H.method := "POST") (\response -> cb $ Right response)
    let rStream = H.requestAsStream req
    _ <- S.writeString rStream Latin1 body (pure unit)
    _ <- S.end rStream (pure unit)
    pure mempty

post' ::
  forall a b
  .  J.WriteForeign a
  => J.ReadForeign b
  => a
  -> Options H.RequestOptions
  -> Aff b
post' body opt = do
  resp <- post (J.writeJSON body) opt
  json <- readToEnd UTF8 (H.responseAsStream resp)
  case J.readJSON json of
    Left err -> throwError (err # head # renderForeignError # error)
    Right o  -> pure o

get :: Options H.RequestOptions -> Aff H.Response
get opt = makeAff go
  where
  go cb = do
    req <- H.request (opt <> H.method := "GET") (\response -> cb $ Right response)
    S.end (H.requestAsStream req) (pure unit)
    pure mempty

get' ::
  forall result
  .  J.ReadForeign result
  => Options H.RequestOptions
  -> Aff result
get' opt = do
  resp <- get opt
  json <- readToEnd UTF8 (H.responseAsStream resp)
  case J.readJSON json of
    Left err -> throwError (err # head # renderForeignError # error)
    Right o  -> pure o
  
foreign import encodeURIComponent_ :: String -> String
encodeURIComponent :: String -> String
encodeURIComponent = encodeURIComponent_
