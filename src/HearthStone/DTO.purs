module Hearthstone.DTO where

type Card
  = { -- flavorText :: String
    image :: String
    , slug :: String
    , id :: Number
    , name :: String
    }

type Pages
  = { page :: Number
    , pageCount :: Number
    , cardCount :: Number
    , cards :: Array Card
    }
