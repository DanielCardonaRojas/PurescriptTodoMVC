module TodoList where
  
import Data.Array ((:))
import Data.Array as Array
import Prelude
import Data.Monoid (guard)
import Halogen as H
import Effect.Console as Console
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Core (ClassName(..), AttrName(..))
import Effect.Aff (Aff)
import Types (Todo(..), Visibility(..))
import TodoForm as Form
import Data.Maybe (Maybe(..), fromMaybe)

type State = { tasks :: Array Todo, visibility :: Visibility }

type Message = Void
data Query a = HandleNewTask (Form.Message) a | UpdateTodo Todo a | DestroyTodo Todo a | ClearCompleted a | ChangeVisibility Visibility a

type FormSlot = String

initialState :: State
initialState =
    { tasks: []
    , visibility: All
    }

component :: H.Component HH.HTML Query Unit Message Aff
component =
    H.parentComponent
        { initialState: const initialState
        , render: render
        , eval: eval
        , receiver: const Nothing
        }

renderTask :: forall m. Todo -> H.ParentHTML Query Form.Query FormSlot m
renderTask (Todo t) =
    let
        label = HH.label [] [HH.text t.taskName]

        deleteButton = HH.button [HP.class_ $ ClassName "destroy", HE.onClick $ HE.input_ (DestroyTodo $ Todo t)] []

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
                -- , HP.checked t.done
                , HE.onChecked $ HE.input (\b -> UpdateTodo $ Todo $ t { done = b })
                ]

        className = if t.done then "completed " else ""
    in
    HH.li
        -- [ HP.class_ $ ClassName ("complete" <> guard t.done "_") ]
        [ HP.class_ $ ClassName className ]
        [ HH.div [ HP.class_ $ ClassName "view" ]
            [ checkbox, label, deleteButton
            ]
        , editInput
        ]

render :: forall m.  State -> H.ParentHTML Query Form.Query FormSlot m
render {tasks: todos, visibility: visibility} =
  let 

    selectedTasks v = 
        case v of
            All -> todos
            Active -> Array.filter (\(Todo t) -> not t.done) todos
            Completed -> Array.filter (\(Todo t) -> t.done) todos

    tasks = selectedTasks All
    tasksCompleted = selectedTasks Completed
    tasksRemaining = selectedTasks Active

    taskList :: H.ParentHTML Query Form.Query FormSlot m
    taskList = HH.ul [HP.class_ $ ClassName "todo-list"] (map renderTask $ selectedTasks visibility)

    taskEntry :: H.ParentHTML Query Form.Query FormSlot m
    taskEntry = HH.slot "Add Todo Form" Form.component unit (HE.input HandleNewTask)

    visibilityControl uri v =
        let
            className = "" <> guard (visibility == v) "selected"
        in
            HH.li
            [ HE.onClick $ HE.input_ (ChangeVisibility v) ]
            [ HH.a [ HP.class_ $ ClassName className, HP.href uri ] [ HH.text $ show v ] ]

    controls =
        HH.footer
            [ HP.class_ $ ClassName "footer"
            -- , hidden (Array.null todos)
            ]
            [ HH.span
                [ HP.class_ $ ClassName "todo-count" ]
                [ HH.strong [] [ HH.text (show $ Array.length tasksRemaining) ]
                , HH.text "items left"
                ]
            , HH.ul
                [ HP.class_ $ ClassName "filters" ]
                [ visibilityControl "#/" All
                , visibilityControl "#/active" Active
                , visibilityControl "#/completed" Completed
                ]
            , HH.button
                [ HP.class_ $ ClassName "clear-completed"
                -- , hidden (Array.null tasksCompleted)
                , HE.onClick $ HE.input_ ClearCompleted 
                ]
                [ HH.text ("Clear completed (" <> show (Array.length tasksCompleted) <> ")") ]
            ]

  in  
  HH.div [HP.class_ $ ClassName "todomvc-wrapper"] 
    [ HH.section [HP.class_ $ ClassName "todoapp"] [taskEntry, taskList, controls]
    ]

eval ::  Query ~> H.ParentDSL State Query Form.Query FormSlot Message Aff
eval query = 
    case query of
        UpdateTodo (Todo todo) next -> do
            H.liftEffect $ Console.log $ show todo
            pendingTasks <- H.gets (_.tasks)
            let updatedTasks = modifyElement (\(Todo t) -> t.taskName == todo.taskName) (\_ -> Todo todo) pendingTasks
            _ <- H.modify (\s -> s {tasks = updatedTasks})
            pure next
        HandleNewTask (Form.NewTodo todo) next -> do
            _ <- H.modify (\s -> s { tasks = todo : s.tasks})
            pure next
        DestroyTodo todo next -> do
            allTasks <- H.gets $ (Array.filter (\t -> t /= todo)) <<< (_.tasks)
            _ <- H.modify (\s -> s { tasks = allTasks })
            pure next
        
        ClearCompleted next -> do
            completed <- H.gets $ (Array.filter (\(Todo t) -> t.done)) <<< (_.tasks)
            let deleteCompleted = Array.takeWhile (\t -> Array.notElem t completed)
            _ <- H.modify (\s -> s {tasks = deleteCompleted s.tasks})
            pure next
        
        ChangeVisibility v next -> do
            _ <- H.modify (\s -> s { visibility = v})
            pure next
        
    where
        modifyElement :: forall e. (e -> Boolean) -> (e -> e) -> Array e -> Array e
        modifyElement pred f array =
            let 
                idx = Array.findIndex pred array # fromMaybe (-1)
                updated = Array.modifyAt idx f array
            in fromMaybe array updated
    
hidden :: forall r i. Boolean -> IProp r i
hidden flag = HP.attr (AttrName "hidden") ("" <> guard flag "hidden")