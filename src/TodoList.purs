module TodoList where
  
import Data.Array ((:))
import Data.Array as Array
import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Types (Todo(..))
import TodoForm as Form
import Data.Maybe (Maybe(..), fromMaybe)

type State = Array Todo
type Message = Void
data Query a = FinishedTodo Todo a | HandleNewTask (Form.Message) a | UpdateTodo Todo a | DestroyTodo Todo a

type FormSlot = String

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
    H.parentComponent
        { initialState: const []
        , render: render
        , eval: eval
        , receiver: const Nothing
        }

renderTask :: forall m. Todo -> H.ParentHTML Query Form.Query FormSlot m
renderTask (Todo t) =
    let
        label = HH.p [] [HH.text t.taskName]
        deleteButton = HH.button [HE.onClick $ HE.input_ (DestroyTodo $ Todo t)] [HH.text "Done"]

        editInput = 
            HH.input 
                [ HP.type_ HP.InputText 
                , HP.class_ $ ClassName "edit"
                , HP.value t.taskName
                , HE.onValueChange $ HE.input_ (UpdateTodo $ Todo t)
                ]

        checkbox = 
            HH.input
            [ HP.class_ $ ClassName "toggle"
            , HP.type_ HP.InputCheckbox
            , HP.checked t.done
            , HE.onChecked $ HE.input (\b -> FinishedTodo $ (Todo $ t { done = b }))
            ]

        className = ""
    in
        HH.li
            [ HP.class_ $ ClassName className ]
            [ HH.div [ HP.class_ $ ClassName "view" ]
                [ checkbox, label, deleteButton
                ]
            , editInput
            ]

render :: forall m.  State -> H.ParentHTML Query Form.Query FormSlot m
render todos =
  let 
    tasks = Array.span (\(Todo todo) -> todo.done) todos

    tasksCompleted = tasks # (_.init)
    tasksRemaining = tasks # (_.rest)

    taskList :: H.ParentHTML Query Form.Query FormSlot m
    taskList = HH.ul [HP.class_ $ ClassName "todo-list"] (map renderTask todos)

    taskEntry :: H.ParentHTML Query Form.Query FormSlot m
    taskEntry = HH.slot "Add Todo Form" Form.component unit (HE.input HandleNewTask)

    controls =
        HH.footer
            [ HP.class_ $ ClassName "footer"
            -- , HP.hidden (Array.null todos)
            ]
            [ HH.span
                [ HP.class_ $ ClassName "todo-count" ]
                [ HH.strong [] [ HH.text (show $ Array.length tasksRemaining) ]
                -- , HH.text (item_ ++ " left")
                ]
            , HH.ul
                [ HP.class_ $ ClassName "filters" ]
                [ 
                -- visibilitySwap "#/" "All" visibility
                -- , visibilitySwap "#/active" "Active" visibility
                -- , visibilitySwap "#/completed" "Completed" visibility
                ]
            , HH.button
                [ HP.class_ $ ClassName "clear-completed"
                -- , HP.hidden (Array.null tasksCompleted)
                -- , HE.onClick $ HE.input_ (DestroyTodo todo)
                ]
                [ HH.text ("Clear completed (" <> show (Array.length tasksCompleted) <> ")") ]
            ]

  in  
  HH.div [HP.class_ $ ClassName "todomvc-wrapper"] 
    [ HH.section [HP.class_ $ ClassName "todoapp"] [taskEntry, taskList, controls]
    ]

eval :: forall m. Query ~> H.ParentDSL State Query Form.Query FormSlot Message m
eval query = 
    case query of
        FinishedTodo todo next -> do
            pendingTasks <- H.get
            let updatedTasks = modifyElement todo (\(Todo t) -> Todo { taskName: t.taskName, done: true } ) pendingTasks
            H.put updatedTasks
            pure next
        HandleNewTask (Form.NewTodo todo) next -> do
            pendingTasks <- H.get
            H.put (todo : pendingTasks)
            pure next
        UpdateTodo todo next -> do
            pure next
        DestroyTodo todo next -> do
            _ <- H.modify (Array.filter (\t -> t /= todo))
            pure next
        
    where
        modifyElement :: forall e. Eq e => e -> (e -> e) -> Array e -> Array e
        modifyElement el f array =
            let 
                idx = Array.elemIndex el array # fromMaybe (-1)
                updated = Array.modifyAt idx f array
            in fromMaybe array updated
    