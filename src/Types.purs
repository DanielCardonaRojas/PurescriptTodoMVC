module Types where
  
import Prelude

newtype Todo = Todo { taskName :: String, done :: Boolean}

derive instance eqTodo :: Eq Todo

data Visibility = All | Completed | Active
derive instance eqVisibility :: Eq Visibility
instance showVisibility :: Show Visibility where
    show v =
        case v of
            All -> "All"
            Completed -> "Completed"
            Active -> "Active"