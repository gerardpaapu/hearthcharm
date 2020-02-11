module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Hearthstone.API as HS
import Main (parseMessage)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)


main :: Effect Unit
main = runTest do
  suite "Heartcharm tests" do
    test "parseMessage" do
      Assert.equal (parseMessage "{c#Cobalt Guardian}") [{ mode: Just HS.Constructed, term: "Cobalt Guardian" }]
      Assert.equal (parseMessage "{b#Cobalt Guardian}") [{ mode: Just HS.Battlegrounds, term: "Cobalt Guardian" }]
      Assert.equal (parseMessage "{Cobalt Guardian}") [{ mode: Nothing, term: "Cobalt Guardian" }]
