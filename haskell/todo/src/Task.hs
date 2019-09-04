module Task
  ( Task(..)
  , Status(..)
  , Progress(..)
  , newTask
  ) where

data Status = Incomplete | Progressing | Complete deriving (Show, Ord, Eq)

data Task = Task { status :: Status
                 , name :: String
                 } deriving (Show, Ord, Eq)

class Progress a where
  isStatus :: Status -> a -> Bool
  getStatus :: a -> Status
  start :: a -> a
  stop :: a -> a
  complete :: a -> a
  isStatus status a = getStatus a == status

instance Progress Task where
  getStatus (Task status _) = status

  start (Task Incomplete name) = Task Progressing name
  start task                   = task

  stop (Task Progressing name) = Task Incomplete name
  stop task                    = task

  complete task = Task Complete $ name task


newTask :: String -> Task
newTask = Task Incomplete