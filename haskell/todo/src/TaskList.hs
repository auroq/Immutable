module TaskList
  ( TaskList
  , addTask
  , removeTask
  , getCompleted
  , getProgressing
  , getIncomplete
  ) where

import Task

type TaskList = [Task]

getCompleted :: TaskList -> TaskList
getCompleted = filter (isStatus Complete)

getProgressing :: TaskList -> TaskList
getProgressing = filter (isStatus Progressing)

getIncomplete :: TaskList -> TaskList
getIncomplete = filter (isStatus Incomplete)

addTask :: String -> TaskList -> TaskList
addTask newTaskName = (:) (newTask newTaskName)

removeTask :: String -> TaskList -> TaskList
removeTask _ [] = []
removeTask expected (task@(Task _ name):tasks)
  | name == expected = tasks
  | otherwise        = task : removeTask expected tasks
