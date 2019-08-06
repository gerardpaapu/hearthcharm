module Hearthcharm.HTTP where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Data.Nullable as N
import Data.Options (Options, (:=))
import Effect.Aff (Aff, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Foreign (renderForeignError)
import Foreign.Object (fromHomogeneous)
import Type.Row.Homogeneous (class Homogeneous)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as H
import Node.Stream as S
import Node.URL as URL
import Query as Q
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

getJSON ::
  forall result query headers
  .  J.ReadForeign result
  => Q.QueryString query
  => Homogeneous headers String
  => String
  -> query
  -> Record headers
  -> Aff result
getJSON url query headers = do
  let qs = Q.toQueryString query
  let obj = URL.parse url
  let options = mempty
              <> (H.protocol := (obj.protocol # orDefault "https:"))
              <> (H.hostname := (obj.hostname # orDefault ""))
              <> (H.path := ((obj.path # orDefault "") <> qs))
              <> (H.headers := H.RequestHeaders (fromHomogeneous headers))
  resp <- get options
  json <- readToEnd UTF8 (H.responseAsStream resp)

  case J.readJSON json of
    Left errs ->
      errs # head # renderForeignError # error # throwError
    Right v ->
      pure v

orDefault :: forall a. a -> N.Nullable a -> a
orDefault a n =
  case N.toMaybe n of
    Nothing -> a
    Just v -> v

foreign import encodeURIComponent_ :: String -> String
encodeURIComponent :: String -> String
encodeURIComponent = encodeURIComponent_
