module Types where
  
import Prelude

newtype Todo = Todo { taskName :: String, done :: Boolean}

derive instance eqTodo :: Eq Todo