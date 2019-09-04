module Task
  ( Task(..)
  , newTask
  , addSubTask
  ) where

data Task = Task { name :: String
                 , subTasks :: [Task]
                 } deriving (Show, Ord, Eq)

newTask :: String -> Task
newTask name = Task name []

addSubTask :: String -> Task -> Task
addSubTask newTaskName (Task name subTasks) = Task name $ newTask newTaskName : subTasks
