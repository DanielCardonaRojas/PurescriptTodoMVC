module Main where

import Prelude
import Effect (Effect)
import TodoList as TodoList
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI TodoList.component unit body