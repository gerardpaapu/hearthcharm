module Query where

import Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (get)
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (reflectSymbol)
import Type.Prelude (class IsSymbol)

type KeyValue
  = { key :: String
    , value :: String
    }

class ToParam a where
  toParam :: a -> String

instance stringToParam :: ToParam String where
  toParam s = s
else instance arrayToParam :: (ToParam a) => ToParam (Array a) where
  toParam s = intercalate "," $ toParam <$> s
else instance symbolToParam :: (IsSymbol a) => ToParam (SProxy a) where
  toParam _ = reflectSymbol (SProxy :: _ a)
else instance maybeToParam :: (ToParam a) => ToParam (Maybe a) where
  toParam (Just v) = toParam v
  toParam Nothing = ""
else instance showToParam ::
 (Show a) => ToParam a where
  toParam s = show s
  
class QueryString a where
  toQueryString :: a -> String

foreign import encodeURIComponent :: String -> String

instance recordToQueryString ::
  ( RowToList fields fieldList
  , WriteQSFields fieldList fields
  ) => QueryString (Record fields) where
  toQueryString rec = "?" <> tail
      where
         pairs = toKeyValuePairs rl rec
         unpair {key, value} =
            encodeURIComponent key <> "=" <> encodeURIComponent value
         tail = intercalate "&" $  unpair <$> pairs
         rl = RLProxy :: RLProxy fieldList

class WriteQSFields (rl :: RowList) row 
  | rl -> row where
  toKeyValuePairs :: forall g. g rl -> Record row -> Array KeyValue

instance consWriteQSFields ::
  ( IsSymbol name
  , Row.Cons name ty whatever row
  , Row.Lacks name whatever
  , ToParam ty
  , WriteQSFields tail row
  ) => WriteQSFields (Cons name ty tail) row where
  toKeyValuePairs _ rec = result
    where
      namep = SProxy :: SProxy name
      value = toParam $ get namep rec
      tailp = RLProxy :: RLProxy tail
      rest = toKeyValuePairs tailp rec
      pair = { key: reflectSymbol namep, value }
      result = [pair] <> rest
      
instance nilWriteQSFields ::
  WriteQSFields Nil row where
  toKeyValuePairs _ _ = []
