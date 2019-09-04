module TaskList
  ( TaskList
  , addTask
  ) where

import Task

type TaskList = [Task]

addTask :: String -> TaskList -> TaskList
addTask newTaskName = (:) (newTask newTaskName)