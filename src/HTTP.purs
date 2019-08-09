module Hearthcharm.HTTP where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Data.Nullable as N
import Data.Options (Options, (:=))
import Effect.Aff (Aff, error, makeAff, throwError)
import Effect.Ref as Ref
import Foreign (renderForeignError)
import Foreign.Object (fromHomogeneous)
import Naporitan (class ReflectProxy)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as H
import Node.Stream (Readable, end, onDataString, onEnd, onError, writeString) as S
import Hearthcharm.Util as Util
import Node.URL as URL
import Prim.Row as Row
import Query as Q
import Simple.JSON as J
import Type.Data.Symbol (SProxy(..), reflectSymbol) as S
import Type.Data.Symbol (class IsSymbol)
import Type.Row.Homogeneous (class Homogeneous)


foreign import kind RequestMethod
foreign import data GetRequest :: RequestMethod
foreign import data PostRequest :: RequestMethod

data Route (method :: RequestMethod) req res (url :: Symbol) = Route

instance routeReflectProxy :: ReflectProxy (Route m i o u) where
  reflectProxy = Route
                    
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
  forall result props props' query  headers urlT
  .  J.ReadForeign result
  => Q.QueryString (Record props)
  => Row.Union props props' query
  => Homogeneous headers String
  => IsSymbol urlT
  => Route GetRequest (Record query) result urlT
  -> Record props
  -> Record headers
  -> Aff result
getJSON _ query headers = do
  let qs = Q.toQueryString query
  let url = S.reflectSymbol (S.SProxy :: _ urlT)
  obj <- url
         # parseURL
         # Util.orThrow ("Invalid URL: " <> show url)
  resp <- get $ (H.protocol := obj.protocol)
                <> (H.hostname := obj.hostname)
                <> (H.path := (obj.path <> qs))
                <> (H.headers := H.RequestHeaders (fromHomogeneous headers))
  json <- readToEnd UTF8 $ H.responseAsStream resp
  case J.readJSON json of
    Left errs ->
      throwError (errs # head # renderForeignError # error)
    Right v ->
      pure v

parseURL :: String -> Maybe { hostname :: String , path :: String , protocol :: String}                   
parseURL source = do
  let obj = URL.parse source
  { protocol: _, hostname: _, path: _ }
    <$> N.toMaybe obj.protocol
    <*> N.toMaybe obj.hostname
    <*> N.toMaybe obj.path
    
orDefault :: forall a. a -> N.Nullable a -> a
orDefault a n =
  case N.toMaybe n of
    Nothing -> a
    Just v -> v

foreign import encodeURIComponent_ :: String -> String
encodeURIComponent :: String -> String
encodeURIComponent = encodeURIComponent_
