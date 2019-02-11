module TodoForm where

import Prelude

import Data.Maybe (Maybe(..))
import Types (Todo(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Web.UIEvent.KeyboardEvent as KE

{- 
Defines a single input field and button to add todo list items
-}

type State
  = String

data Message
  = NewTodo Todo

data Query a
  = AddedTodo a
  | UpdatedName String a
  | NoOp a

component :: forall m. H.Component HH.HTML Query Unit Message m
component = H.component { initialState: const ""
                        , eval
                        , render
                        , receiver: const Nothing
                        }

render :: State -> H.ComponentHTML Query
render state = 
    let 
        taskNameInputField = 
            HH.input 
                [ HP.class_ $ ClassName "new-todo"
                , HP.type_ HP.InputText
                , HP.autofocus true
                , HP.placeholder "What needs to be done?"
                , HE.onValueChange $ HE.input UpdatedName
                , HE.onKeyUp $ HE.input (\k -> if KE.ctrlKey k then AddedTodo else NoOp )
                , HP.value state
                ]
                   
        addButton = HH.button 
            [ HE.onClick $ HE.input_ AddedTodo] 
            [ HH.text "Add"]

    in HH.div [] [taskNameInputField, addButton]


eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval query = case query of
    AddedTodo next -> do
        taskName <- H.get 
        H.put "" 
        H.raise $ NewTodo $ Todo { taskName, done: false }
        pure next

    UpdatedName todoName next -> do
        H.put todoName
        pure next
    
    NoOp next -> do
        pure next
