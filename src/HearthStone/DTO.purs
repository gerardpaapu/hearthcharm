module Hearthstone.DTO where

import Data.Maybe (Maybe)

type Card
  = { -- flavorText :: String
    image :: String
    , slug :: String
    , id :: Number
    , name :: String
    , battlegrounds :: Maybe { hero :: Boolean
                             , image :: String
                             }
    }

type Pages
  = { page :: Number
    , pageCount :: Number
    , cardCount :: Number
    , cards :: Array Card
    }
